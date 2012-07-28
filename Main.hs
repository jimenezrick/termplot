module Main (main) where

import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Control.Exception
import Data.List.Split
import Graphics.Vty

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
addValChart v (BarChart (n, vs)) | length vs == maxChartVals = BarChart (n, v:init vs)
                                 | otherwise                 = BarChart (n, v:vs)

readChartNames :: Handle -> IO [String]
readChartNames hdl = liftM (splitOn ",") $ hGetLine hdl

readChartVals :: Handle -> IO [Double]
readChartVals = liftM (map read) . readChartNames

composeChartImg :: BarChart -> Image
composeChartImg (BarChart (name, vals)) =
    string def_attr name <|> string def_attr (getBars vals)

updateUi :: Vty -> [BarChart] -> [Double] -> IO [BarChart]
updateUi vty bcs vs = do
    let bcs' = zipWith addValChart vs bcs
    drawChart vty bcs'
    return bcs'

drawChart :: Vty -> [BarChart] -> IO ()
drawChart vty bcs = update vty pic
    where bcs' = map composeChartImg bcs
          img = foldr (<->) empty_image bcs'
          pic = pic_for_image img

runUi :: Handle -> IO ()
runUi hdl = bracket mkVty shutdown $ runUi' hdl

runUi' :: Handle -> Vty -> IO ()
runUi' hdl vty = do
    charts <- fmap (map newBarChart) (readChartNames hdl)
    drawChart vty charts
    runUi'' hdl vty charts

runUi'' :: Handle -> Vty -> [BarChart] -> IO ()
runUi'' hdl vty bcs = do
    vals <- readChartVals hdl
    bcs' <- updateUi vty bcs vals
    runUi'' hdl vty bcs'
