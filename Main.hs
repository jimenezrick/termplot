import Text.Printf

import Data.Time.Units
import Control.Monad
import Control.Concurrent

import Control.Monad.IO.Class
import Data.Default

import Graphics.Vty
import Brick (Widget)
import qualified Brick as B

import Options.Applicative
import System.Process (readCreateProcessWithExitCode, shell)
import System.Exit

import System.Posix.IO
import System.Posix.Terminal
import Data.List.Split

data AppEvent = Tick
              | Terminal Event





main :: IO ()
main = do
    isatty <- queryTerminal stdInput
    if isatty
        then execParser opts >>= runApp
        else nonInteractivePlot
  where
    parser = App <$> (many $ strArgument (
                     metavar "CMD" <>
                         help "Command to run"))
                 <*> pure []
                 <*> pure []
                 <*> pure 1
                 <*> pure False
                 <*> pure 1024
    opts = info (helper <*> parser) mempty



nonInteractivePlot :: IO ()
nonInteractivePlot = do
    series <- map (map toDouble . concatMap (splitOn ",") . splitOn " ") . lines <$> getContents
    if all ((== 1) . length) series
        then putStrLn $ getBars $ concat series
        else mapM_ (putStrLn . getBars) series

toDouble :: String -> Double
toDouble s = case reads s of
        [(v, _)] | v >= 0    -> v
                 | otherwise -> error $ printf "negative numeric value: %v" s
        _                    -> error $ printf "invalid numeric value: %v" s




runCmd :: String -> IO String
runCmd cmd = do
    (exit, stdout, _) <- readCreateProcessWithExitCode (shell cmd) mempty
    case exit of
        ExitFailure _ -> error $ "Command failed: " ++ cmd
        ExitSuccess   -> return stdout





runApp :: App -> IO ()
runApp app = do
    let termApp = B.App {
        B.appDraw = \a -> [renderApp a]
      , B.appHandleEvent = loopApp
      , B.appStartEvent = return
      , B.appAttrMap = def
      , B.appLiftVtyEvent = Terminal
      , B.appChooseCursor = B.neverShowCursor
      }
    ticker <- runTicker $ appRefreshRate app
    void $ B.customMain (mkVty def) ticker termApp (initApp app)
    return ()


data App = App {
    appCmds :: [String]
   ,appCmdOuts :: [[Double]]
   ,appCmdNames :: [String]
   , appRefreshRate :: Second
   , appLogScale :: Bool
   , appSerieSize :: Int
  } deriving Show

renderApp :: App -> Widget
renderApp app = seriesNames B.<+> lastValues B.<+> (B.vBox $ map (B.str . getBars . transform) $ appCmdOuts app)
  where seriesNames = B.padRight (B.Pad 1 ) $ B.vBox (map B.str (appCmdNames app))
        lastValues  = B.vBox (map (B.str . showSeries) (appCmdOuts app))
        showSeries [] = ""
        showSeries s  = show $ head s
        transform | appLogScale app = logScale
                  | otherwise       = id


loopApp :: App -> AppEvent -> B.EventM (B.Next App)
loopApp app (Terminal (EvKey (KChar c) [MCtrl])) = B.halt app
loopApp app (Terminal (EvKey KEsc _))            = B.halt app
loopApp app _                                    = do
    newOuts <- liftIO $ mapM (uncurry update1) $ zip (appCmds app) (appCmdOuts app)
    B.continue $ app { appCmdOuts = newOuts }






update1 :: String -> [Double] -> IO [Double]
update1 cmd vals = do
    cmdOut <- runCmd cmd
    case reads cmdOut of
        [(v, _)] | v >= 0    -> return $ take maxxx $ v:vals
                 | otherwise -> error $ printf "negative numeric value: %v (from: %v)" cmdOut cmd
        _                    -> error $ printf "invalid numeric value: %v (from: %v)" cmdOut cmd

maxxx :: Int
maxxx = 1024


initApp :: App -> App
initApp app =
    let rawCmds       = appCmds app
        (names, cmds) = unzip $ map splitNameCmd rawCmds
        emptySeries   = replicate (length rawCmds) []
    in app { appCmdNames = names, appCmds = cmds, appCmdOuts = emptySeries}

splitNameCmd :: String -> (String, String)
splitNameCmd s = case span (/= ':') s of
                       (name, cmd) | null name || null cmd -> error $ printf "missing name for command: %v" s
                                   | length cmd < 2        -> error $ printf "missing command: %v" s
                                   | otherwise             -> (name, tail cmd)





runTicker :: Second -> IO (Chan AppEvent)
runTicker rate = do
    ch <- newChan
    void $ forkIO $ forever $ do
        writeChan ch Tick
        threadDelay $ fromInteger $ toMicroseconds rate
    return ch

barChars :: String
barChars = " ▁▂▃▄▅▆▇█"

getBar :: Double -> Double -> Double -> Char
getBar min' max' n | min' == max' = barChars !! (round $ (fromIntegral $ length barChars) / 2)
                   | otherwise    =
    let len = fromIntegral $ length barChars
        wid = (max' - min') / (len - 1)
        idx = round $ (n - min') / wid
    in barChars !! idx

getBars :: [Double] -> String
getBars l = map (getBar (minimum l) (maximum l)) l

logScale :: [Double] -> [Double]
logScale = map (logBase 10 . (+1))
