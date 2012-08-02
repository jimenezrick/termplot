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
import Data.Either
import Graphics.Vty hiding (pad)

{-
 - TODO: /proc charts (i.e. CPU)
 -}

main :: IO ()
main = do
    args <- getArgs
    case args of
      []           -> usage
      ["-h"]       -> usage
      ["-f", pipe] -> withFile pipe ReadMode runUi
      _            -> do
          let seqs = map readValSeq args
          case lefts seqs of
            []    -> putStrLn . getBars . concat . rights $ seqs
            err:_ -> printError err

printError :: String -> IO ()
printError ""  = return ()
printError err = do
    hPutStrLn stderr $ "Error: " ++ err
    exitFailure

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

readValSeq :: Read a => String -> Either String [a]
readValSeq s =
    case reads $ "[" ++ s ++ "]" :: Read a => [(a, String)] of
      [(x, "")] -> Right x
      _         -> Left "invalid input"

readChartVals :: Handle -> IO (Either String [Double])
readChartVals hdl = do
    s <- hGetLine hdl
    return $ readValSeq s

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
runUi hdl = do
    err <- bracket mkVty shutdown runUi'
    printError err
    exitSuccess
        where runUi' vty = do
                mvar <- newEmptyMVar
                t1 <- forkIO (inputReader vty mvar)
                t2 <- forkIO (fifoReader mvar hdl)
                runUi'' vty mvar t1 t2
              runUi'' vty mvar t1 t2 = do
                var <- takeMVar mvar
                case var of
                  Left err -> do
                      killThread t1
                      killThread t2
                      return err
                  Right cs -> do
                      drawChart vty cs
                      runUi'' vty mvar t1 t2

inputReader :: Vty -> MVar (Either String a) -> IO ()
inputReader vty mvar = do
    ev <- next_event vty
    case ev of
      EvKey (KASCII 'c') [MCtrl] -> putMVar mvar $ Left ""
      _                          -> inputReader vty mvar

fifoReader :: MVar (Either String [BarChart]) -> Handle -> IO ()
fifoReader mvar hdl =
    let handler :: IOError -> IO ()
        handler e | isEOFError e = putMVar mvar $ Left ""
                  | otherwise    = ioError e
    in handle handler $ do
        charts <- fmap (map newBarChart) (readChartNames hdl)
        putMVar mvar $ Right charts
        loop charts
            where loop cs = do
                    vs <- readChartVals hdl
                    case vs of
                      Left err  -> putMVar mvar $ Left err
                      Right vs' -> do
                          let cs' = zipWith addValChart vs' cs
                          putMVar mvar $ Right cs'
                          loop cs'
