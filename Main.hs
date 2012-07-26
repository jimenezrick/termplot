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

readBarNames :: Handle -> IO [String]
readBarNames hdl = (hGetLine hdl) >>= (return . (splitOn ","))

readBarsVal :: Handle -> IO [Double]
readBarsVal = liftM (map read) . readBarNames

composeBar :: BarLine -> Image
composeBar (BarLine (name, vals)) = string def_attr name <|> string def_attr (getBars vals)

runUi :: Handle -> IO ()
runUi hdl = bracket mkVty shutdown $ runUi' hdl

runUi' :: Handle -> Vty -> IO ()
runUi' hdl vty = do
    bars <- fmap (map newBarLine) (readBarNames hdl)
    vals <- sequence $ repeat $ readBarsVal hdl



    {-uncurry updateUi-}




    -- XXX HLint


    return ()







updateUi :: Vty -> [BarLine] -> IO ()
updateUi vty bls = update vty pic
    where pic = pic_for_image img
          img = foldr (\b i -> b <|> i) empty_image bls'
          bls' = map composeBar bls
