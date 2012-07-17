module Main where

import System.Environment
import Data.List.Split
{-import Graphics.Vty-}

{-import Control.Concurrent-}
{-main2 :: IO ()-}
{-main2 = do-}
    {-vty <- mkVty-}
    {-update vty $ pic_for_image $ string def_attr " ▁▂▃▄▅▆▇"-}
    {-[>refresh vty<]-}
    {-threadDelay $ 1000 * 1000-}
    {-shutdown vty-}
    {-putStrLn "End"-}

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> return ()
      _  -> do
          let seq = map read $ concat $ map (splitOn ",") args :: [Double]
          putStrLn $ getBars seq

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
