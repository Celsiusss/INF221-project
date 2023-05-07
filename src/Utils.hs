{-# LANGUAGE ImportQualifiedPost #-}

module Utils where

import Control.Lens
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Ix
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.String.Utils
import System.Process.Typed qualified as P
import Text.Printf (PrintfArg, printf)
import Text.Read
import Types

type ClockTickRate = Integer

-- takes a number that represents kilobytes and gives a formatted string
formatMemory :: (Integral a, Ix a, Show a, PrintfArg a) => a -> String
formatMemory bytes
  | inRange (0, 1024) bytes = show kb ++ " kB"
  | inRange (1024, 1024 ^ 2) bytes = printf "%.f" (kb / 1024) ++ " MB"
  | inRange (1024 ^ 2, 1024 ^ 3) bytes = printf "%.2f" (kb / 1024 ^ 2) ++ " GB"
  | otherwise = printf "%.2f" (kb / 1024 ^ 3) ++ " TB"
  where
    kb = fromIntegral bytes :: Double

parseProcess :: [String] -> Process
parseProcess stats =
  Process
    (read $ stats !! 0)
    ""
    (take 15 (stats !! 1))
    (read (stats !! 23) * 4)
    (sum $ map (\i -> read (stats !! i)) [13, 14, 15, 16])
    0
    0

-- parse stat given by /proc/PID/stat
parseStat :: String -> [String]
parseStat s = parseStat_ [] s

parseStat_ :: [String] -> String -> [String]
parseStat_ rest "" = rest
parseStat_ rest (' ' : str) = parseStat_ rest str
parseStat_ rest (')' : str) = parseStat_ rest str
parseStat_ rest ('(' : str) = parseStat_ (rest ++ [takeWhile (/= ')') str]) $ dropWhile (/= ')') str
parseStat_ rest str = parseStat_ (rest ++ [takeWhile (/= ' ') str]) $ dropWhile (/= ' ') str

-- returns the sum of all cpu times, to get total cpu time
parseCpuStat :: String -> Integer
parseCpuStat = sum . map read . drop 1 . words . head . lines

-- get the clock tick rate (aka Hz) that is used for calculating cpu times
getSystemClockTick :: IO Integer
getSystemClockTick = do
  (exitCode, out, err) <- P.readProcess "getconf CLK_TCK"
  return $ (read . unpack) out

getPIDOwners :: [PID] -> IO [String]
getPIDOwners pids = do
  let conf = P.proc "stat" $ ["-c", "%U"] ++ map (\p -> "/proc/" ++ show p) pids
  (exitCode, out, err) <- P.readProcess conf
  return $ lines (unpack out)

calculateCpuTime :: Integer -> Integer -> ClockTickRate -> Integer -> Integer -> Integer
calculateCpuTime utime stime ctr uptime starttime =
  let totaltime = utime + stime
      seconds = uptime - starttime `div` ctr
   in 100 * ((totaltime `div` ctr) `div` seconds)

orderProcesses :: OrderBy -> Bool -> [Process] -> [Process]
orderProcesses orderBy True processes = sortBy (processOrdering orderBy) processes
orderProcesses orderBy False processes = sortBy (flip (processOrdering orderBy)) processes

processOrdering :: OrderBy -> Process -> Process -> Ordering
processOrdering OrderByPID p1 p2 = compare (p1 ^. pid) (p2 ^. pid)
processOrdering OrderByName p1 p2 = compare (p1 ^. name) (p2 ^. name)
processOrdering OrderByMemory p1 p2 = compare (p1 ^. memory) (p2 ^. memory)
processOrdering OrderByCPU p1 p2 = compare (p1 ^. cpuPerc) (p2 ^. cpuPerc)
processOrdering OrderByOwner p1 p2 = compare (p1 ^. owner) (p2 ^. owner)

updateHist :: Double -> [Double] -> [Double]
updateHist new hist
  | length hist >= 100 = tail hist ++ [new]
  | otherwise = hist ++ [new]
