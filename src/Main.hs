{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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


import CellularAutomata

drawGrid::CA (w c) c=>(w c)-> [Widget Name]
drawGrid grid = [withBorderStyle BS.unicodeBold
            $ B.borderWithLabel (str "Conway's Game of Life")
            $ vBox rows]
        where
            rows = [hBox $ rowCells r | r <- [1..100]] 
            rowCells  y = [drawCell x | x <- gridRow y grid]
            

drawCell :: CA w c=>c-> Widget ()
drawCell cell = withAttr (cellAtt cell) cw





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

app::(Comonad w, CA (w c) c)=>App (w c) Tick Name
app = App{  appDraw = drawGrid
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const attMap}


--handleEvent::CA w a=>w a->BrickEvent Name Tick->EventM Name (Next (w a))
handleEvent::(Comonad w, CA (w c) c)=>(w c)->BrickEvent Name Tick -> EventM Name (Next (w c))
handleEvent g (AppEvent Tick) = continue $ extend next_gen g

main :: IO ()
main = do
    chan <- newBChan 10
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 100
    void $ customMain (V.mkVty V.defaultConfig) (Just chan) app start_state_brian