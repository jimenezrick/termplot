module Main (main) where

import Prelude hiding (catch)
import System.IO
import System.Exit
import System.Environment
import System.IO.Error hiding (catch)
import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.List.Split
import Graphics.Vty hiding (pad)

{-
 - TODO: Catch read error
 -       CPU chart
 -       Haskell project structure
 -}

main :: IO ()
main = do
    args <- getArgs
    case args of
      []           -> usage
      ["-h"]       -> usage
      ["-f", pipe] -> withFile pipe ReadMode runUi
      _            -> do
          let seq' = map read $ concatMap (splitOn ",") args :: [Double]
          putStrLn $ getBars seq'

usage :: IO ()
usage = do
    prog <- getProgName
    hPutStrLn stderr $ "Usage: " ++ prog ++ " -h | -f <fifo> | <sequence>"
    exitFailure

barChars :: String
barChars = " ▁▂▃▄▅▆▇█"

getBar :: Double -> Double -> Double -> Char
getBar min' max' n =
    let len = fromIntegral $ length barChars
        wid = (max' - min') / (len - 1)
        idx = round $ (n - min') / wid
    in barChars !! idx

getBars :: [Double] -> String
getBars l = map (getBar (minimum l) (maximum l)) l

newtype BarChart = BarChart (String, [Double])

newBarChart :: String -> BarChart
newBarChart n = BarChart (n, [])

maxChartVals :: Int
maxChartVals = 1024

addValChart :: Double -> BarChart -> BarChart
addValChart v (BarChart (n, vs))
    | length vs == maxChartVals = BarChart (n, v:init vs)
    | otherwise                 = BarChart (n, v:vs)

readChartNames :: Handle -> IO [String]
readChartNames hdl = liftM (splitOn ",") $ hGetLine hdl

readChartVals :: Handle -> IO [Double]
readChartVals = liftM (map read) . readChartNames

composeChartImg :: BarChart -> Image
composeChartImg (BarChart (name, vals)) =
    string def_attr name <|> string def_attr (getBars vals)

padChartNames :: [BarChart] -> [BarChart]
padChartNames cs = map (\(BarChart (s, vs)) -> BarChart (pad s, vs)) cs
    where len   = maximum $ map (\(BarChart (s, _)) -> length s) cs
          pad s = s ++ replicate (len - length s + 1) ' '

tryLogScale :: BarChart -> BarChart
tryLogScale b@(BarChart (s, vs))
    | all (> 0) vs = BarChart (s, map (logBase 10) vs)
    | otherwise   = b

drawChart :: Vty -> [BarChart] -> IO ()
drawChart vty cs = update vty pic
    where cs' = map (composeChartImg . tryLogScale) $ padChartNames cs
          img = foldr (<->) empty_image cs'
          pic = pic_for_image img

runUi :: Handle -> IO ()
runUi hdl = bracket mkVty shutdown runUi'
    where runUi' vty = do
            mvar <- newEmptyMVar
            t1 <- forkIO (inputReader vty mvar)
            t2 <- forkIO (fifoReader mvar hdl)
            runUi'' vty mvar t1 t2
          runUi'' vty mvar t1 t2 = do
            var <- takeMVar mvar
            case var of
              Nothing -> do
                  killThread t1
                  killThread t2
                  exitSuccess
              Just cs -> do
                  drawChart vty cs
                  runUi'' vty mvar t1 t2

inputReader :: Vty -> MVar (Maybe a) -> IO ()
inputReader vty mvar = do
    ev <- next_event vty
    case ev of
      EvKey (KASCII 'c') [MCtrl] -> putMVar mvar Nothing
      _                          -> inputReader vty mvar

fifoReader :: MVar (Maybe [BarChart]) -> Handle -> IO ()
fifoReader mvar hdl =
    let handler :: IOError -> IO ()
        handler e | isEOFError e = putMVar mvar Nothing
                  | otherwise    = ioError e
    in handle handler $ do
        charts <- fmap (map newBarChart) (readChartNames hdl)
        putMVar mvar $ Just charts
        loop charts
            where loop cs = do
                    vs <- readChartVals hdl
                    let cs' = zipWith addValChart vs cs
                    putMVar mvar $ Just cs'
                    loop cs'
