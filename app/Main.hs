{-# LANGUAGE OverloadedStrings #-}

module Main where

import Shapes.Shape
import Shapes.Render
import Web.Scotty
import qualified Web.Scotty as S
import Network.Wai.Middleware.Static
import Text.Blaze.Svg.Renderer.Text

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as Map

drawingFromText :: String -> Drawing
drawingFromText str = read str

main = scotty 3002 $ do
    middleware $ staticPolicy (noDots >-> addBase "public")

    get "/" $ do file "./public/index.html"
    post "/svg" $ do
        drawingText <- (S.param "drawingText") `rescue` return
        let drawing = drawingFromText $ (T.unpack . TL.toStrict) drawingText
        S.html $ renderSvg $ toSvg drawing
