module Main (main) where

import System.IO
import System.Exit
import System.Environment
import Control.Exception
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
          let seq' = map read $ concatMap (splitOn ",") args :: [Double]
          putStrLn $ getBars seq'

usage :: IO ()
usage = do
    prog <- getProgName
    hPutStrLn stderr $ "Usage: " ++ prog ++ " -h | -p <pipe> | <sequence>"
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









newtype BarLine = BarLine (String, [Double])

newBarLine :: String -> BarLine
newBarLine n = BarLine (n, [])

maxBarVals :: Int
maxBarVals = 1024

addValBar :: Double -> BarLine -> BarLine
addValBar v (BarLine (n, vs)) | length vs == maxBarVals = BarLine (n, v:init vs)
                              | otherwise               = BarLine (n, v:vs)












{-updateBarLines :: Vty -> [BarLine] -> IO ()-}
{-updateBarLines vty bls = do-}
    {-DisplayRegion w h <- display_bounds $ terminal vty-}




{-updateUi :: Vty -> IO ()-}
{-updateUi vty =-}



runUi :: Handle -> IO ()
runUi hdl = bracket mkVty shutdown $ runUi' hdl

runUi' :: Handle -> Vty -> IO ()
runUi' hdl vty = do
    bars <- fmap (map newBarLine) (readBarNames hdl)









    {-cont <- hGetContents hdl-}
    {-mapM_ (\c -> update vty $ pic_for_image $ string def_attr [getBar 0 10 (read c :: Double)]) (lines cont)-}






readBarNames :: Handle -> IO [String]
readBarNames hdl = (hGetLine hdl) >>= (return . (splitOn ","))



composeBar :: BarLine -> Image
composeBar BarLine (name, vals) = string def_attr name <|> string def_attr $ getBars vals




drawBars :: [BarLine] -> IO ()
