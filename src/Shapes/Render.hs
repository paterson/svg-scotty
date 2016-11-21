{-# LANGUAGE OverloadedStrings #-}

module Shapes.Render where

import Shapes.Shape

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m, toValue)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Printf (printf)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List (intersperse)

svgHeader = S.docTypeSvg ! A.version "1.1" ! A.width "250" ! A.height "250" ! A.viewbox "0 0 250 250"

toSvg :: Drawing -> S.Svg
toSvg drawing = svgHeader $ foldl1 (>>) $ map elementToSvg drawing

elementToSvg :: (Transform, Shape, Stylesheet) -> S.Svg
elementToSvg (transform, shape, stylesheet) =
    applyTransformAttributes (generateTransform transform) $ do
        foldl (!) (shapeToSvg shape) (translateStylesheet stylesheet)

applyTransformAttributes :: [S.Attribute] -> S.Svg -> S.Svg
applyTransformAttributes [] svg     = S.g svg                   -- Finally, apply the svg for shape and stylesheet
applyTransformAttributes (x:xs) svg = S.g ! x $ do              -- Apply each transform  and nest the next one
    (applyTransformAttributes xs svg)

-- Example output to allow multiple transforms:

-- elementToSvg :: (Transform, Shape, Stylesheet) -> S.Svg
-- elementToSvg (transform, shape, stylesheet) =
--     S.g ! A.transform (rotate 50) $ do
--         S.g ! A.transform (Translate (Vector 50 50)) $ do
--             foldl (!) (foldl (!) (shapeToSvg shape) (translateStylesheet stylesheet)) (generateTransform transform)


shapeToSvg :: Shape -> S.Svg
shapeToSvg Square = S.rect
shapeToSvg Circle = S.circle

translateStylesheet :: Stylesheet -> [S.Attribute]
translateStylesheet sheet = map styleToAttribute sheet

generateTransform :: Transform -> [S.Attribute]
generateTransform transform = map A.transform (transformToAttributeValues transform)

styleToAttribute :: Style -> S.Attribute
styleToAttribute (Stroke c)  = A.stroke      $ toValue $ colorToString c
styleToAttribute (Fill c)    = A.fill        $ toValue $ colorToString c
styleToAttribute (Outline f) = A.strokeWidth $ toValue f
styleToAttribute (X f)       = A.x           $ toValue f
styleToAttribute (Y f)       = A.y           $ toValue f
styleToAttribute (Width f)   = A.width       $ toValue f
styleToAttribute (Height f)  = A.height      $ toValue f
styleToAttribute (CenterX f) = A.cx          $ toValue f
styleToAttribute (CenterY f) = A.cy          $ toValue f
styleToAttribute (Radius f)  = A.r           $ toValue f

transformToAttributeValues :: Transform -> [S.AttributeValue]
transformToAttributeValues Identity                 = []
transformToAttributeValues (Rotate x)               = [S.rotate x]
transformToAttributeValues (Translate (Vector x y)) = [S.translate x y]
transformToAttributeValues (Scale (Vector x y))     = [S.scale x y]
transformToAttributeValues (Compose x y)            = (transformToAttributeValues x) ++ (transformToAttributeValues y)

colorToString :: Color -> String
colorToString (RGB r g b) = printf "rgb(%d,%d,%d)" r g b
colorToString (Hex hex)   = hex
colorToString c           = show c
