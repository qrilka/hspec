{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Hspec.Core.Formatters.Internal (
  FormatM
, FormatConfig(..)
, runFormatM
, interpret
, increaseSuccessCount
, increasePendingCount
, addFailMessage
, finally_
, formattersToFormat
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import qualified System.IO as IO
import           System.IO (Handle)
import           Control.Exception (AsyncException(..), bracket_, try, throwIO)
import           System.Console.ANSI
import           Control.Monad.Trans.State hiding (state, gets, modify)
import           Control.Monad.IO.Class
import           Data.Char (isSpace)
import qualified System.CPUTime as CPUTime

import qualified Test.Hspec.Core.Formatters.Monad as M
import           Test.Hspec.Core.Formatters.Monad (Environment(..), interpretWith, FailureRecord(..))
import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Clock

formattersToFormat :: [(M.Formatter, Handle, Bool)] -> FormatConfig -> Format FormatM
formattersToFormat formatters config = Format {
  formatRun = \action -> runFormatM config $ do
    interpretFormatters M.headerFormatter
    a <- action `finally_` interpretFormatters M.failedFormatter
    interpretFormatters $ M.footerFormatter
    return a
, formatGroupStarted = \ (nesting, name) ->
    interpretFormatters $ \formatter -> M.exampleGroupStarted formatter nesting name
, formatGroupDone = \ _ -> interpretFormatters M.exampleGroupDone
, formatProgress = \ path progress -> {-when useColor $-} do
    interpretFormatters $ \formatter -> M.exampleProgress formatter path progress
, formatItem = \ path (Item loc _duration info result) -> do
    forM_ formatters $ \(_, h, _) ->
        clearTransientOutput h
    case result of
      Success -> do
        increaseSuccessCount
        interpretFormatters $ \formatter -> M.exampleSucceeded formatter path info
      Pending reason -> do
        increasePendingCount
        interpretFormatters $ \formatter -> M.examplePending formatter path info reason
      Failure err -> do
        addFailMessage loc path err
        interpretFormatters $ \formatter -> M.exampleFailed formatter path info err
} where
    interpretFormatters action = do
      forM_ formatters $ \(f, h, c) ->
        interpret h c $ action f

interpret :: Handle -> Bool -> M.FormatM a -> FormatM a
interpret h useColor = interpretWith Environment {
  environmentGetSuccessCount = getSuccessCount
, environmentGetPendingCount = getPendingCount
, environmentGetFailMessages = getFailMessages
, environmentUsedSeed = usedSeed
, environmentGetCPUTime = getCPUTime
, environmentGetRealTime = getRealTime
, environmentWrite = write h
, environmentWriteTransient = writeTransient h
, environmentWithFailColor = withFailColor h useColor
, environmentWithSuccessColor = withSuccessColor h useColor
, environmentWithPendingColor = withPendingColor h useColor
, environmentWithInfoColor = withInfoColor h useColor
, environmentUseDiff = gets (formatConfigUseDiff . stateConfig)
, environmentExtraChunk = extraChunk h useColor
, environmentMissingChunk = missingChunk h useColor
, environmentLiftIO = liftIO
}

-- | A lifted version of `Control.Monad.Trans.State.gets`
gets :: (FormatterState -> a) -> FormatM a
gets f = FormatM $ do
  f <$> (get >>= liftIO . readIORef)

-- | A lifted version of `Control.Monad.Trans.State.modify`
modify :: (FormatterState -> FormatterState) -> FormatM ()
modify f = FormatM $ do
  get >>= liftIO . (`modifyIORef'` f)

data FormatConfig = FormatConfig {
  formatConfigUseDiff :: Bool
, formatConfigHtmlOutput :: Bool
, formatConfigPrintCpuTime :: Bool
, formatConfigUsedSeed :: Integer
} deriving (Eq, Show)

data FormatterState = FormatterState {
  stateSuccessCount    :: Int
, statePendingCount    :: Int
, stateFailMessages    :: [FailureRecord]
, stateCpuStartTime    :: Maybe Integer
, stateStartTime       :: Seconds
, stateTransientOutput :: String
, stateConfig :: FormatConfig
}

getConfig :: (FormatConfig -> a) -> FormatM a
getConfig f = gets (f . stateConfig)

-- | The random seed that is used for QuickCheck.
usedSeed :: FormatM Integer
usedSeed = getConfig formatConfigUsedSeed

-- NOTE: We use an IORef here, so that the state persists when UserInterrupt is
-- thrown.
newtype FormatM a = FormatM (StateT (IORef FormatterState) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runFormatM :: FormatConfig -> FormatM a -> IO a
runFormatM config (FormatM action) = do
  time <- getMonotonicTime
  cpuTime <- if (formatConfigPrintCpuTime config) then Just <$> CPUTime.getCPUTime else pure Nothing
  st <- newIORef (FormatterState 0 0 [] cpuTime time "" config)
  evalStateT action st

-- | Increase the counter for successful examples
increaseSuccessCount :: FormatM ()
increaseSuccessCount = modify $ \s -> s {stateSuccessCount = succ $ stateSuccessCount s}

-- | Increase the counter for pending examples
increasePendingCount :: FormatM ()
increasePendingCount = modify $ \s -> s {statePendingCount = succ $ statePendingCount s}

-- | Get the number of successful examples encountered so far.
getSuccessCount :: FormatM Int
getSuccessCount = gets stateSuccessCount

-- | Get the number of pending examples encountered so far.
getPendingCount :: FormatM Int
getPendingCount = gets statePendingCount

-- | Append to the list of accumulated failure messages.
addFailMessage :: Maybe Location -> Path -> FailureReason -> FormatM ()
addFailMessage loc p m = modify $ \s -> s {stateFailMessages = FailureRecord loc p m : stateFailMessages s}

-- | Get the list of accumulated failure messages.
getFailMessages :: FormatM [FailureRecord]
getFailMessages = reverse `fmap` gets stateFailMessages

writeTransient :: Handle -> String -> FormatM ()
writeTransient h s = do
  write h ("\r" ++ s)
  modify $ \ state -> state {stateTransientOutput = s}
  liftIO $ IO.hFlush h

clearTransientOutput :: Handle -> FormatM ()
clearTransientOutput h = do
  n <- length <$> gets stateTransientOutput
  unless (n == 0) $ do
    write h ("\r" ++ replicate n ' ' ++ "\r")
    modify $ \ state -> state {stateTransientOutput = ""}

-- | Append some output to the report.
write :: Handle -> String -> FormatM ()
write h s = liftIO $ IO.hPutStr h s

-- | Set output color to red, run given action, and finally restore the default
-- color.
withFailColor :: Handle -> Bool -> FormatM a -> FormatM a
withFailColor h useColor = withColor h useColor (SetColor Foreground Dull Red) "hspec-failure"

-- | Set output color to green, run given action, and finally restore the
-- default color.
withSuccessColor :: Handle -> Bool -> FormatM a -> FormatM a
withSuccessColor h useColor = withColor h useColor (SetColor Foreground Dull Green) "hspec-success"

-- | Set output color to yellow, run given action, and finally restore the
-- default color.
withPendingColor :: Handle -> Bool -> FormatM a -> FormatM a
withPendingColor h useColor = withColor h useColor (SetColor Foreground Dull Yellow) "hspec-pending"

-- | Set output color to cyan, run given action, and finally restore the
-- default color.
withInfoColor :: Handle -> Bool -> FormatM a -> FormatM a
withInfoColor h useColor = withColor h useColor (SetColor Foreground Dull Cyan) "hspec-info"

-- | Set a color, run an action, and finally reset colors.
withColor :: Handle -> Bool -> SGR -> String -> FormatM a -> FormatM a
withColor h useColor color cls action = do
  produceHTML <- getConfig formatConfigHtmlOutput
  (if produceHTML then htmlSpan h cls else withColor_ h useColor color) action

htmlSpan :: Handle -> String -> FormatM a -> FormatM a
htmlSpan h cls action = write h ("<span class=\"" ++ cls ++ "\">") *> action <* write h "</span>"

withColor_ :: Handle -> Bool -> SGR -> FormatM a -> FormatM a
withColor_ h useColor color (FormatM action) =
  FormatM . StateT $ \st -> do
    bracket_

      -- set color
      (when useColor $ hSetSGR h [color])

      -- reset colors
      (when useColor $ hSetSGR h [Reset])

      -- run deeper action
      (runStateT action st)

-- | Output given chunk in red.
extraChunk :: Handle -> Bool -> String -> FormatM ()
extraChunk h useColor s = do
  useDiff <- getConfig formatConfigUseDiff
  case useDiff of
    True -> extra s
    False -> write h s
  where
    extra :: String -> FormatM ()
    extra = diffColorize h useColor Red "hspec-failure"

-- | Output given chunk in green.
missingChunk :: Handle -> Bool -> String -> FormatM ()
missingChunk h useColor s = do
  useDiff <- getConfig formatConfigUseDiff
  case useDiff of
    True -> missing s
    False -> write h s
  where
    missing :: String-> FormatM ()
    missing = diffColorize h useColor Green "hspec-success"

diffColorize :: Handle -> Bool -> Color -> String -> String-> FormatM ()
diffColorize h useColor color cls s = withColor h useColor (SetColor layer Dull color) cls $ do
  write h s
  where
    layer
      | all isSpace s = Background
      | otherwise = Foreground

-- |
-- @finally_ actionA actionB@ runs @actionA@ and then @actionB@.  @actionB@ is
-- run even when a `UserInterrupt` occurs during @actionA@.
finally_ :: FormatM a -> FormatM () -> FormatM a
finally_ (FormatM actionA) (FormatM actionB) = FormatM . StateT $ \st -> do
  r <- try (runStateT actionA st)
  case r of
    Left e -> do
      when (e == UserInterrupt) $
        runStateT actionB st >> return ()
      throwIO e
    Right (a, st_) -> do
      runStateT actionB st_ >>= return . replaceValue a
  where
    replaceValue a (_, st) = (a, st)

-- | Get the used CPU time since the test run has been started.
getCPUTime :: FormatM (Maybe Seconds)
getCPUTime = do
  t1  <- liftIO CPUTime.getCPUTime
  mt0 <- gets stateCpuStartTime
  return $ toSeconds <$> ((-) <$> pure t1 <*> mt0)
  where
    toSeconds x = Seconds (fromIntegral x / (10.0 ^ (12 :: Integer)))

-- | Get the passed real time since the test run has been started.
getRealTime :: FormatM Seconds
getRealTime = do
  t1 <- liftIO getMonotonicTime
  t0 <- gets stateStartTime
  return (t1 - t0)
