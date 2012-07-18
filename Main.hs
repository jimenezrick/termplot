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
      ["-h"]       -> usage
      ["-p", pipe] -> runUi pipe
      _            -> do
          let seq = map read $ concat $ map (splitOn ",") args :: [Double]
          putStrLn $ getBars seq

usage :: IO ()
usage = do
    prog <- getProgName
    hPutStrLn stderr $ "Usage: " ++ prog ++ " -h | <sequence>"
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





runUi :: FilePath -> IO ()
runUi f = do
    handle <- openFile f ReadMode
    vty <- mkVty
    cont <- hGetContents handle
    mapM_ (\c -> update vty $ pic_for_image $ string def_attr [getBar 0 10 (read c :: Double)]) (lines cont)
    shutdown vty
