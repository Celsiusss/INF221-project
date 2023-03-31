{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text, pack)
import GHC.IO.FD (openFile)
import Monomer
import System.Directory
import Text.Read (readMaybe)
import TextShow

data Process = Process
  { _pid :: Int,
    _name :: String,
    _memory :: Int
  }
  deriving (Eq, Show)

data AppModel = AppModel
  { _processes :: [Process]
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppUpdate [Process]
  | AppIncrease
  | AppReadProc
  deriving (Eq, Show)

makeLenses 'AppModel
makeLenses 'Process

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vscroll $
        vstack
          [ label "Hello world",
            spacer,
            hstack [],
            hgrid [label "PID", label "Name"],
            vgrid $
              map (\x -> hgrid [label $ showt $ x ^. pid, label $ pack $ x ^. name]) $
                model ^. processes
          ]
          `styleBasic` [padding 10]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Task $ AppUpdate <$> readProcess]
  AppReadProc -> [Task $ AppUpdate <$> readProcess]
  AppUpdate s -> [Model $ model & processes .~ s]
  AppIncrease -> [Model model]

readProcess :: IO [Process]
readProcess = do
  names <- listDirectory "/proc"
  let pids = mapMaybe readMaybe names
  names <- mapM (\p -> readFile $ "/proc/" ++ show p ++ "/comm") pids
  stats <- mapM (\p -> readFile $ "/proc/" ++ show p ++ "/stat") pids
  let stats' = map words stats -- todo
  let processes = zipWith (\p name -> Process p name 0) pids names
  return processes

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Hello world",
        appWindowIcon "./assets/images/icon.png",
        appTheme lightTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppReadProc
      ]
    model = AppModel []
