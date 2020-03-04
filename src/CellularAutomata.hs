{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module CellularAutomata where

import Data.Matrix
import Control.Comonad
import System.Console.ANSI
import Control.Concurrent
import System.Random
import Control.Monad.State

sizeX::Int
sizeX = 300

sizeY::Int
sizeY = 300


type Point = (Int, Int)

data Cell = Alive | Dead deriving(Eq)

data Brian = ALV | DED | DYING deriving(Eq)

data Grid a = Grid (Matrix a) Point deriving(Show, Functor)


myRand :: State StdGen Int
myRand = do
    gen <- get
    let (r, nextGen) = randomR (1,300) gen
    put nextGen
    return r

randPair::State StdGen (Int,Int)
randPair = do
            x<-myRand
            y<-myRand
            return (x,y)

cells = take 4500 $ evalState (mapM (\_ -> randPair) $ repeat ()) $ mkStdGen 1
gun = [(3,8),(3,9),(4,8),(4,9),(13,8),(13,9),(13,10),(14,7),
         (14,8),(15,6),(15,12),(16,6),(16,12),(17,9),(18,7),
         (18,11),(19,8),(19,9),(19,10),(20,9),(23,6),(23,7),
         (23,8),(24,6),(24,7),(24,8),(25,5),(25,9),(27,4),
         (27,5),(27,9),(27,10),(37,6),(37,7),(38,6),(38,7)]  

--cells = gun ++ map (\(x,y)->(x,y+15)) gun ++ map (\(x,y)->(x,y+30)) gun



neighbors = [(x,y) | x <- [-1,0,1], y <-[-1,0,1], (x,y) /= (0,0)]

instance Show Cell where
  show Alive = "@"
  show Dead  = " "

instance Show Brian where
  show ALV = "#"
  show DED = " "
  show DYING = "1"


instance Comonad Grid where
  -- extract:: w a -> a
  extract (Grid mtx (x,y)) = getElem x y mtx
  -- extend::(w a -> b)->w a->w b
  extend f = fmap f . duplicate
  -- duplicate::w a -> w(w a) 
  duplicate (Grid mtx (x,y)) = Grid (matrix sizeX sizeY (\(w,z)->(Grid mtx (w,z)))) (x,y)


class Comonad w=> CA w c where
  next_gen::w c->c
  neighborValues::w c->[c]
  start_state::w c
  play::Int->w c->IO()
  showGrid::w c->String


getNeighbors::Grid a->[(Int, Int)]
getNeighbors (Grid mtx (x,y))=filter (\(a,b)->if a==0||b==0||a>sizeX||b>sizeY then False else True) (map (\(w,z)->(w+x, y+z)) neighbors)


instance CA Grid Cell where
  start_state = Grid (matrix sizeX sizeY (\(x,y)->if (any ((x,y)==) cells) then Alive else Dead)) (5,1)
  next_gen (Grid mtx (x,y)) = case (getElem x y mtx) of
                                  Alive -> case (length(filter(\x->x==Alive) (neighborValues (Grid mtx (x,y))))) of
                                            2-> Alive
                                            3-> Alive
                                            _-> Dead
                                  Dead -> case (length(filter(\x->x==Alive) (neighborValues (Grid mtx (x,y))))) of 
                                            3-> Alive
                                            _-> Dead
  neighborValues (Grid mtx (x,y)) = fmap (\(y,z)->getElem y z mtx) $ getNeighbors (Grid mtx (x,y))
  play 0 grid = do
              clearScreen
              threadDelay 5000
              putStr $ showGrid grid
  play n grid = do
              clearScreen
              threadDelay 5000
              putStr $ showGrid grid
              play (n-1) (extend next_gen grid)
           
  showGrid (Grid mtx p) = show mtx

instance CA Grid Brian where
  start_state = Grid (matrix sizeX sizeY (\(x,y)->if (any ((x,y)==) cells) then ALV else DED)) (5,1)
  next_gen (Grid mtx (x,y)) = case (getElem x y mtx) of
                                      ALV -> DYING
                                      DED -> case (length(filter(\x->x==ALV) (neighborValues (Grid mtx (x,y))))) of
                                            2->ALV
                                            _->DED
                                      DYING -> DED 
  neighborValues (Grid mtx (x,y)) = fmap (\(y,z)->getElem y z mtx) $ getNeighbors (Grid mtx (x,y))
  play 0 grid = do
                clearScreen
                threadDelay 500000
                putStr $ showGrid grid
  play n grid = do
                clearScreen
                threadDelay 500000
                putStr $ showGrid grid
                play (n-1) (extend next_gen grid)
  showGrid (Grid mtx p) = show mtx

