{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Comonad
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Brick
    ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
    , customMain, neverShowCursor
    , continue, halt
    , hLimit, vLimit, vBox, hBox
    , padRight, padLeft, padTop, padAll, Padding(..)
    , withBorderStyle
    , str
    , attrMap, withAttr, emptyWidget, AttrName, on, fg
    , (<+>)
    )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))
import Data.Matrix
import Data.Vector as Vec (toList)  

import CellularAutomata

drawGrid::Grid Brian-> [Widget Name]
drawGrid (Grid mtx p) = [withBorderStyle BS.unicodeBold
            $ B.borderWithLabel (str "Conway's Game of Life")
            $ vBox rows]
        where
            rows = [hBox $ rowCells r | r <- [1..100]] 
            rowCells  y = [drawCell2 x | x <- Vec.toList (getRow y mtx)]
            

drawCell :: Cell -> Widget ()
drawCell Alive = withAttr aliveAttr cw
drawCell Dead  = withAttr deadAttr cw

drawCell2::Brian -> Widget ()
drawCell2 ALV = withAttr alvAttr cw
drawCell2 DED = withAttr dedAttr cw
drawCell2 DYING = withAttr dyingAttr cw


cw::Widget ()
cw = str " "


aliveAttr, deadAttr::AttrName
aliveAttr = "aliveAttr"::AttrName
deadAttr = "deadAttr"::AttrName
alvAttr = "alvAttr"::AttrName
dedAttr = "dedAttr"::AttrName
dyingAttr = "dyingAttr"::AttrName

attMap::AttrMap
attMap = attrMap V.defAttr
    [
        (aliveAttr, V.red `on` V.red)   
    ,   (deadAttr, V.yellow `on` V.yellow)
    ,   (alvAttr, V.blue `on` V.blue)
    ,   (dedAttr, V.black `on` V.black)
    ,   (dyingAttr, V.white `on` V.white)
    ]


data Tick = Tick

type Name = ()

app::App (Grid Brian) Tick Name
app = App{  appDraw = drawGrid
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const attMap}


handleEvent::Grid Brian->BrickEvent Name Tick->EventM Name (Next (Grid Brian))
handleEvent g (AppEvent Tick) = continue $ extend next_gen g

main :: IO ()
main = do
    chan <- newBChan 10
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 100
    void $ customMain (V.mkVty V.defaultConfig) (Just chan) app (start_state::Grid Brian)