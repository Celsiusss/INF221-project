module LineGraphWidget where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad
import Data.Default
import Data.Text (pack)
import Data.Typeable (cast)
import Debug.Trace (trace)
import Monomer
import Monomer.Common.BasicTypes
import Monomer.Lens qualified as L
import Monomer.Widgets.Single
import Types (AppEvent (AppReadProc), AppModel)

data LineGraphCfg = LineGraphCfg
  { lgcLineColor :: Color,
    lgcBgColor :: Color,
    lgcTitle :: Maybe String
  }

instance Default LineGraphCfg where
  def =
    LineGraphCfg
      { lgcLineColor = blue,
        lgcBgColor = black,
        lgcTitle = Nothing
      }

data LineGraphState = LineGraphState
  { gData :: [Double]
  }
  deriving (Eq, Show)

instance Default LineGraphState where
  def = LineGraphState []

newtype LineGraphMessage = UpdateGraph [Double]
  deriving (Eq, Show)

lineGraph :: LineGraphCfg -> [Double] -> WidgetNode s e
lineGraph config hist = defaultWidgetNode "lineGraph" widget
  where
    widget = makeLineGraph (LineGraphState hist) config

makeLineGraph :: LineGraphState -> LineGraphCfg -> Widget s e
makeLineGraph state config = widget
  where
    widget =
      createSingle
        state
        def
          { singleRender = render state config,
            singleGetSizeReq = getSizeReq
          }

    getSizeReq :: WidgetEnv s e -> WidgetNode s e -> (SizeReq, SizeReq)
    getSizeReq wenv node = (flexWidth 1, height 100)

    render :: LineGraphState -> LineGraphCfg -> WidgetEnv s e -> WidgetNode s e -> Renderer -> IO ()
    render state config wenv node renderer = do
      let style = currentStyle wenv node
      let area = getContentArea node style
      let farCorner = Point (_rW area) (_rH area)
      let hist = gData state
      let textArea = area {_rH = 15}

      drawRect renderer area (Just black) Nothing
      drawInTranslation renderer (Point (_rX area) (_rY area)) $ do
        drawGraph renderer hist (_pX farCorner) (_pY farCorner) (Just $ lgcLineColor config)
      drawRectBorder renderer area (Border Nothing Nothing Nothing Nothing) Nothing
      case lgcTitle config of
        Just title -> drawTextLine renderer style (def {_tlText = pack title, _tlFontSize = FontSize 15, _tlRect = textArea})
        Nothing -> pure ()

    drawGraph :: Renderer -> [Double] -> Double -> Double -> Maybe Color -> IO ()
    drawGraph renderer hist width height color = do
      let xs =
            map
              ( \i ->
                  fromIntegral i / fromIntegral (length hist) * width
              )
              [0 .. length hist]
      let ys = map (\y -> height - y / 100 * height) hist
      mapM_
        ( \((y0, y1), (x0, x1)) ->
            drawLine renderer (Point x0 y0) (Point x1 y1) 1 color
        )
        $ zip (zip (init ys) (tail ys)) (zip (init xs) (tail xs))

    merge wenv node oldNode oldState = resultNode newNode
      where
        newNode = node & L.widget .~ makeLineGraph oldState config
