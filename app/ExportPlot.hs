{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ExportPlot(Color,writePlotData) where

import Data.Aeson
import GHC.Generics
import Data.ByteString.Lazy as BS (writeFile)
import Text.Printf (printf)

type Color = (Int, Int, Int)

-- Define data types to represent the JSON structure

data Point = Point
  { x :: Int
  , y :: Double
  , text :: [String]
  } deriving (Show, Generic)

data Dataset = Dataset
  { label :: String
  , dataPoints :: [Point]
  , backgroundColor :: String
  , borderColor :: String
  , pointRadius :: Int
  } deriving (Show, Generic)

data Root = Root
  { datasets :: [Dataset]
  } deriving (Show, Generic)

instance ToJSON Point
-- Custom ToJSON instance for Dataset to change "dataPoints" to "data" in JSON
instance ToJSON Dataset where
  toJSON Dataset{..} = object
    [ "label" .= label
    , "data" .= dataPoints -- Changed field name in JSON output
    , "backgroundColor" .= backgroundColor
    , "borderColor" .= borderColor
    , "pointRadius" .= pointRadius
    ]
instance ToJSON Root

makePoint :: (Int, Double, [String]) -> Point
makePoint (iX,iY,iText) = Point
    { x = iX
    , y = iY
    , text = iText
    }

makeColor :: Color -> Double -> String
makeColor (r,g,b) alpha = printf "rgba(%d, %d, %d, %.1f)" r g b alpha

makeDataset :: (String, Color, [(Int, Double, [String])]) -> Dataset
makeDataset (lb,clr,pts) = Dataset
    { label = lb
    , dataPoints = map makePoint pts
    , backgroundColor = makeColor clr 0.6
    , borderColor = makeColor clr 1
    , pointRadius = 5
    }

-- Write plot data to file as json
writePlotData :: FilePath -> [(String, Color, [(Int, Double, [String])])] -> IO ()
writePlotData filePath plotData = BS.writeFile filePath (encode root)
    where root = Root {datasets = map makeDataset plotData}