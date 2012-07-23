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

nameBarLines :: [String] -> [BarLine]
nameBarLines = map (\n -> BarLine (n, []))

maxSavedValues :: Int
maxSavedValues = 1024

addValue :: Int -> BarLine -> BarLine
addValue v (BarLine s vs) | length vs > maxSavedValues = BarLine s v:(init vs)
                          | otherwise                  = BarLine s v:vs












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
