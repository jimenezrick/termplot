module Main where

import Graphics.Vty
import Control.Concurrent

main2 :: IO ()
main2 = do
    vty <- mkVty
    update vty $ pic_for_image $ string def_attr " ▁▂▃▄▅▆▇"
    {-refresh vty-}
    threadDelay $ 1000 * 1000
    shutdown vty
    putStrLn "End"





bars :: String
bars = " ▁▂▃▄▅▆▇█"

getBar :: Double -> Double -> Char
getBar max n =
    let len = length bars
        wid = fromIntegral len / (max + 1)
        idx = round $ wid * n
    in bars !! (min idx (len - 1))

getBars :: [Double] -> String
getBars l = map (getBar $ maximum l) l
