module Shapes.Render where

import Shapes.Shape

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m, toValue)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List (intersperse)

svgHeader = S.docTypeSvg ! A.version (toValue "1.1") ! A.width (toValue "250") ! A.height (toValue "250") ! A.viewbox (toValue "0 0 250 250")

toSvg :: Drawing -> S.Svg
toSvg drawing = svgHeader $ foldl1 (>>) $ map elementToSvg drawing

elementToSvg :: (Transform, Shape, Stylesheet) -> S.Svg
elementToSvg (transform, shape, stylesheet) =
    (foldl (!) (shapeToSvg shape) (translateStylesheet stylesheet)) ! generateTransform transform

shapeToSvg :: Shape -> S.Svg
shapeToSvg Square = S.rect
shapeToSvg Circle = S.circle

translateStylesheet :: Stylesheet -> [S.Attribute]
translateStylesheet sheet = map styleToAttribute sheet

generateTransform :: Transform -> S.Attribute
generateTransform Identity  = A.transform $ S.rotate 0
generateTransform transform = A.transform $ (transformToAttributeValue transform) !! 0

styleToAttribute :: Style -> S.Attribute
styleToAttribute (Stroke c)  = A.stroke      $ toValue $ show c
styleToAttribute (Fill c)    = A.fill        $ toValue $ show c
styleToAttribute (Outline f) = A.strokeWidth $ toValue f
styleToAttribute (X f)       = A.x           $ toValue f
styleToAttribute (Y f)       = A.y           $ toValue f
styleToAttribute (Width f)   = A.width       $ toValue f
styleToAttribute (Height f)  = A.height      $ toValue f
styleToAttribute (CenterX f) = A.cx          $ toValue f
styleToAttribute (CenterY f) = A.cy          $ toValue f
styleToAttribute (Radius f)  = A.r           $ toValue f

transformToAttributeValue :: Transform -> [S.AttributeValue]
transformToAttributeValue Identity                 = []
transformToAttributeValue (Rotate x)               = [S.rotate x]
transformToAttributeValue (Translate (Vector x y)) = [S.translate x y]
transformToAttributeValue (Scale (Vector x y))     = [S.scale x y]
transformToAttributeValue (Compose x y)            = (transformToAttributeValue x) ++ (transformToAttributeValue y)
