module UI where

import Control.Lens
import Data.Default
import Data.Text (pack)
import LineGraphWidget
import Monomer
import Text.Printf
import TextShow
import Types
import Utils

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = ui
  where
    ui =
      vscroll_
        [wheelRate 60]
        $ vstack
          [ vstack
              [ lineGraph (def {lgcLineColor = green, lgcTitle = Just "CPU Usage"}) (model ^. cpuHistory),
                lineGraph (def {lgcLineColor = blue, lgcTitle = Just "Memory Usage"}) (model ^. memHistory)
              ],
            hgrid
              [ label $ pack $ "Total CPU usage: " ++ printf "%0.1f" ((sum . map (^. cpuPerc)) (model ^. processes)),
                label $ pack $ "Memory usage: " ++ formatMemory (model ^. memTotal - model ^. memFree) ++ " / " ++ formatMemory (model ^. memTotal)
              ],
            spacer,
            hstack [],
            hgrid
              [ button "PID" (AppSortBy OrderByPID),
                button "Owner" (AppSortBy OrderByOwner),
                button "Name" (AppSortBy OrderByName),
                button "Memory" (AppSortBy OrderByMemory),
                button "CPU" (AppSortBy OrderByCPU)
              ],
            vgrid $ map makeRow (orderProcesses (model ^. orderBy) (model ^. orderAscending) (model ^. processes))
          ]
          `styleBasic` [padding 10]

    makeRow :: Process -> WidgetNode s e
    makeRow p =
      hgrid $
        map
          label
          [ showt $ p ^. pid,
            pack $ p ^. owner,
            pack $ p ^. name,
            pack $ formatMemory (p ^. memory),
            pack (printf "%0.1f" $ p ^. cpuPerc :: String)
          ]
