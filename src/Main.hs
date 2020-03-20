{-# LANGUAGE OverloadedStrings #-}
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
import Data.Vector as Vec
import CellularAutomata

drawGrid::(Comonad w, Cell c, CA (w c) c)=>(w c)-> [Widget Name]
drawGrid grid = [withBorderStyle BS.unicodeBold
            $ B.borderWithLabel (str (getName grid))
            $ vBox rows]
        where
            rows = [hBox $ rowCells r | r <- [1..100]] 
            rowCells  y = [drawCell x | x <- Vec.toList ( getRow y (fmap cellAttr (getMatrix grid)))]
            

drawCell::AttrName-> Widget ()
drawCell cell = withAttr cell cw

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
        (aliveAttr, V.blue `on` V.blue)   
    ,   (deadAttr, V.black `on` V.black)
    ,   (dyingAttr, V.white `on` V.white)
    ]


data Tick = Tick

type Name = ()

app::(Comonad w, Cell c, CA (w c) c)=>App (w c) Tick Name
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
    void $ customMain (V.mkVty V.defaultConfig) (Just chan) app start_state_conway