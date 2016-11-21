module Shapes.Shape(
  Shape (..), Point, Vector (..), Transform (..), Drawing, Stylesheet, Style (..), Color (..), (<+>)
) where

import Data.Map (Map)
import qualified Data.Map as Map

-- Utilities
data Vector = Vector Double Double
              deriving (Show, Read)

-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving (Show, Read)
-- Styles
data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Orange
           | Magenta
           | Cyan
           | White
           | RGB Int Int Int
           | Hex String
             deriving (Show, Read)

data Style = Stroke Color
           | Fill Color
           | Outline Float
           | X Float
           | Y Float
           | Width Float
           | Height Float
           | CenterX Float
           | CenterY Float
           | Radius Float
             deriving (Show, Read)

type Stylesheet = [Style]

-- Shapes

type Point  = Vector
data Shape = Empty
           | Circle
           | Square
             deriving (Show, Read)

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Double
             deriving (Show, Read)

t0 <+> t1 = Compose t0 t1

-- Drawings

type Drawing = [(Transform,Shape, Stylesheet)]
