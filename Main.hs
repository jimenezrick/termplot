module Main (main) where

import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.List.Split
import Graphics.Vty

{-
 - TODO: Terminate on EOF and Ctrl-C
 -       CPU chart
 -       Haskell project structure
 -       Row name padding
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

drawChart :: Vty -> [BarChart] -> IO ()
drawChart vty cs = update vty pic
    where cs' = map composeChartImg cs
          img = foldr (<->) empty_image cs'
          pic = pic_for_image img

runUi :: Handle -> IO ()
runUi hdl = bracket mkVty shutdown $ runUi'
    where runUi' vty = do
            mvar <- newEmptyMVar
            _ <- forkIO (inputReader vty mvar)
            _ <- forkIO (fifoReader mvar hdl)
            runUi'' vty mvar
          runUi'' vty mvar = do
            var <- takeMVar mvar
            case var of
              Nothing -> return ()
              Just cs -> do
                  drawChart vty cs
                  runUi'' vty mvar

inputReader :: Vty -> MVar (Maybe a) -> IO ()
inputReader vty mvar = do
    ev <- next_event vty
    case ev of
      EvKey (KASCII 'c') [MCtrl] -> putMVar mvar Nothing
      _                          -> inputReader vty mvar

fifoReader :: MVar (Maybe [BarChart]) -> Handle -> IO ()
fifoReader mvar hdl = do
    charts <- fmap (map newBarChart) (readChartNames hdl)
    putMVar mvar $ Just charts
    loop charts
        where loop cs = do
                vs <- readChartVals hdl
                let cs' = zipWith addValChart vs cs
                putMVar mvar $ Just cs'
                loop cs'
