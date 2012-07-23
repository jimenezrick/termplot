module Main where

import System.IO
import System.Exit
import System.Environment
import Data.List.Split
import Graphics.Vty

main :: IO ()
main = do
    args <- getArgs
    case args of
      []           -> usage
      ["-h"]       -> usage
      ["-p", pipe] -> withFile pipe ReadMode runUi
      _            -> do
          let seq = map read $ concat $ map (splitOn ",") args :: [Double]
          putStrLn $ getBars seq

usage :: IO ()
usage = do
    prog <- getProgName
    hPutStrLn stderr $ "Usage: " ++ prog ++ " -h | -p <pipe> | <sequence>"
    exitFailure

bars :: String
bars = " ▁▂▃▄▅▆▇█"

getBar :: Double -> Double -> Double -> Char
getBar min max n =
    let len = fromIntegral $ length bars
        wid = (max - min) / (len - 1)
        idx = round $ (n - min) / wid
    in bars !! idx

getBars :: [Double] -> String
getBars l = map (getBar (minimum l) (maximum l)) l









newtype BarLine = BarLine (String, [Double])

newBarLine :: String -> BarLine
newBarLine n = BarLine (n, [])

maxBarVals :: Int
maxBarVals = 1024

addValBar :: Int -> BarLine -> BarLine
addValBar v (BarLine (n, vs)) | length vs == maxBarVals = BarLine n v:(init vs)
                              | otherwise               = BarLine n v:vs












updateBarLines :: Vty -> [BarLine] -> IO ()
updateBarLines vty bls = do
    DisplayRegion w h <- display_bounds $ terminal vty




{-updateUi :: Vty -> IO ()-}
{-updateUi vty =-}






runUi :: Handle -> IO ()
runUi h = do
    vty <- mkVty
    cont <- hGetContents h
    mapM_ (\c -> update vty $ pic_for_image $ string def_attr [getBar 0 10 (read c :: Double)]) (lines cont)
    shutdown vty
