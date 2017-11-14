import Brick
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.List.Split
import Data.Time.Units
import Graphics.Vty hiding ((<|>))
import Options.Applicative hiding (str)
import System.Exit
import System.Posix.IO
import System.Posix.Terminal
import System.Process
import Text.Printf

data AppEvent = Tick
              | Terminal Event

data AppState = AppState {
    appCmds        :: [String]
  , appCmdOuts     :: [[Double]]
  , appCmdNames    :: [String]
  , appRefreshRate :: Second
  , appLogScale    :: Bool
  , appSerieSize   :: Int
  } deriving Show

main :: IO ()
main = execParser opts >>= runApp
  where parser = AppState <$> many (strArgument (
                                    metavar "CMDs"
                                 <> help "Commands to plot output ([<name>:<cmd> ...])"))
                          <*> pure []
                          <*> pure []
                          <*> ((toPosNum <$> strOption (
                                    short 'i'
                                 <> long "interval"
                                 <> metavar "INTERVAL"
                                 <> help "Updates interval time (needs units: 's', 'm', 'h')"))
                              <|> pure 1)
                          <*> switch (
                                    short 'l'
                                 <> long "log"
                                 <> help "Use logarithmic scale")
                          <*> ((toPosNum <$> strOption (
                                    short 's'
                                 <> long "size"
                                 <> metavar "SIZE"
                                 <> help "Size of the time series buffer"))
                              <|> pure 1024)
        opts = info (helper <*> parser) mempty

nonInteractivePlot :: Bool -> IO ()
nonInteractivePlot useLog = do
    series <- map (map toPosNum . concatMap (splitOn ",") . splitOn " ") . lines <$> getContents
    if all ((== 1) . length) series
        then putStrLn $ getBars' $ concat series
        else mapM_ (putStrLn . getBars') series
  where getBars' | useLog    = getBars . logScale
                 | otherwise = getBars

toPosNum :: (Ord a, Num a, Read a) => String -> a
toPosNum s = case reads s of
        [(v, _)] | v >= 0    -> v
                 | otherwise -> error $ printf "negative numeric value: %v" s
        _                    -> error $ printf "invalid numeric value: %v" s
runCmd :: String -> IO String
runCmd cmd = do
    (exit, stdout, _) <- readCreateProcessWithExitCode (shell cmd) mempty
    case exit of
        ExitFailure _ -> error $ "Command failed: " ++ cmd
        ExitSuccess   -> return stdout

runApp :: AppState -> IO ()
runApp app = do
    let termApp         = App {
        appDraw         = \a -> [renderApp a]
      , appHandleEvent  = loopApp
      , appStartEvent   = return
      , appAttrMap      = def
      , appLiftVtyEvent = Terminal
      , appChooseCursor = neverShowCursor
      }
    isatty <- queryTerminal stdInput
    if not isatty
        then nonInteractivePlot $ appLogScale app
        else
            if null $ appCmds app
                then error "no command to execute specified nor input from the stdin"
                else do
                    ticker <- runTicker $ appRefreshRate app
                    void $ customMain (mkVty def) ticker termApp (initApp app)

renderApp :: AppState -> Widget
renderApp app = seriesNames <+> lastValues <+> vBox (map (str . getBars') $ appCmdOuts app)
  where seriesNames                 = pad1 $ vBox (map str (appCmdNames app))
        lastValues                  = pad1 $ vBox (map (str . showSeries) (appCmdOuts app))
        pad1                        = padRight $ Pad 1
        showSeries []               = ""
        showSeries s                = show $ head s
        getBars' | appLogScale app  = getBars . logScale
                 | otherwise        = getBars

loopApp :: AppState -> AppEvent -> EventM (Next AppState)
loopApp app (Terminal (EvKey (KChar 'c') [MCtrl])) = halt app
loopApp app (Terminal (EvKey KEsc _))              = halt app
loopApp app _                                      = do
    newOuts <- liftIO $ zipWithM (runAppCmd maxLen) (appCmds app) (appCmdOuts app)
    continue $ app { appCmdOuts = newOuts }
  where maxLen = appSerieSize app

runAppCmd :: Int -> String -> [Double] -> IO [Double]
runAppCmd maxLen cmd vals = do
    cmdOut <- runCmd cmd
    case reads cmdOut of
        [(v, _)] | v >= 0    -> return $ take maxLen $ v:vals
                 | otherwise -> error $ printf "negative numeric value: %v (from: %v)" cmdOut cmd
        _                    -> error $ printf "invalid numeric value: %v (from: %v)" cmdOut cmd

initApp :: AppState -> AppState
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
getBar min' max' n | min' == 0
                   , max' == 0    = head barChars
                   | min' == max' = barChars !! round (fromIntegral (length barChars) / 2 :: Double)
                   | otherwise    =
    let len = fromIntegral $ length barChars
        wid = (max' - min') / (len - 1)
        idx = round $ (n - min') / wid
    in barChars !! idx

getBars :: [Double] -> String
getBars l = map (getBar (minimum l) (maximum l)) l

logScale :: [Double] -> [Double]
logScale = map (logBase 10 . (+1))
