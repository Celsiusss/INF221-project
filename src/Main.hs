{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use map once" #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Lens
import Control.Monad (forever)
import Data.Char (isSpace)
import Data.Default (def)
import Data.List (find)
import Data.Maybe
import Monomer
import System.Directory (listDirectory)
import System.IO.Strict qualified as Strict (readFile)
import Text.Printf (printf)
import Text.Read (readMaybe)
import TextShow
import Types
import UI (buildUI)
import Utils

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Producer refresher]
  AppReadProc -> [Task $ AppTick <$> doProcessUpdate model]
  AppUpdate ps -> [Model $ model & processes .~ ps]
  AppIncrease -> [Model model]
  AppTick (ps, t, (mTotal, mFree)) ->
    [ Model $
        model
          & processes .~ ps
          & previousCpuTimeTotal .~ (model ^. cpuTimeTotal)
          & cpuTimeTotal .~ t
          & cpuHistory .~ newCpuHist
          & memTotal .~ mTotal
          & memFree .~ mFree
          & memHistory .~ newMemHist
    ]
    where
      totalCpuPerc = (sum . map (^. cpuPerc)) (model ^. processes)
      newCpuHist = updateHist totalCpuPerc $ model ^. cpuHistory
      memPerc = 100 * (fromIntegral (model ^. memTotal - model ^. memFree) :: Double) / fromIntegral (model ^. memTotal)
      newMemHist = updateHist memPerc $ model ^. memHistory
  AppSortBy ordering ->
    [ Model $
        if currentOrdering == ordering
          then model & orderAscending .~ not (model ^. orderAscending)
          else model & orderBy .~ ordering
    ]
    where
      currentOrdering = model ^. orderBy
      currentIsAscending = model ^. orderAscending

refresher :: (AppEvent -> IO ()) -> IO ()
refresher sendMsg = forever $ do
  sendMsg AppReadProc
  threadDelay (1000 * 1000)

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path = either (const Nothing) Just <$> (try (Strict.readFile path) :: IO (Either IOException String))

readProcesses :: IO [Process]
readProcesses = do
  names <- listDirectory "/proc"
  let pids = mapMaybe readMaybe names :: [Int]
  owners <- getPIDOwners pids
  stats <- mapM (\p -> readFileMaybe $ "/proc/" ++ show p ++ "/stat") pids
  ctr <- getSystemClockTick
  let processes = map (parseProcess . parseStat) (catMaybes stats)
  let withOwners = zipWith (\s p -> p & owner .~ s) owners processes
  return withOwners

readMemory :: IO (Int, Int)
readMemory = do
  out <- readFile "/proc/meminfo"
  let list = map (map (takeWhile (/= ':')) . filter (\s -> s /= "" && s /= "kB") . words) $ lines out

  let mappedList = map (\[a, b] -> (a, read b :: Int)) list
  let findVal s = (fmap snd . find (\(name, value) -> name == s)) mappedList

  let total = findVal "MemTotal"
  let free = findVal "MemAvailable"

  return (fromJust total, fromJust free)

readTotalCpuTime :: IO Integer
readTotalCpuTime = parseCpuStat <$> readFile "/proc/stat"

-- takes previous processes as input, and return a tuple of new processes, prev cpu times and total cpu time
doProcessUpdate :: AppModel -> IO ([Process], Integer, (Int, Int))
doProcessUpdate model = do
  let previousProcesses = model ^. processes
  processes <- readProcesses
  totalCpuTime <- readTotalCpuTime

  -- map cpuTime from previousProcesses into prevCpuTime on newProcesses
  let newProcesses =
        map (applyPreviousCpuTimes previousProcesses . calculateCpuTimes totalCpuTime (model ^. previousCpuTimeTotal)) $
          map (applyPreviousCpuTimes previousProcesses) processes

  memory <- readMemory
  return (newProcesses, totalCpuTime, memory)
  where
    applyPreviousCpuTimes :: [Process] -> Process -> Process
    applyPreviousCpuTimes previousProcesses process =
      process & prevCpuTime .~ maybe 0 (^. cpuTime) (find (\p' -> (p' ^. pid) == (process ^. pid)) previousProcesses)

    calculateCpuTimes :: Integer -> Integer -> Process -> Process
    calculateCpuTimes totalCpuTime previousTotalCpuTime process =
      process & cpuPerc .~ (2 * 100 * (fromInteger (process ^. cpuTime - process ^. prevCpuTime) :: Double) / fromInteger (totalCpuTime - previousTotalCpuTime))

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Hello world",
        appWindowIcon "./assets/images/icon.png",
        appTheme lightTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppInit
      ]
    model = def
