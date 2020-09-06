{-# LANGUAGE OverloadedStrings #-}
module EscapeTheRoom where

import           CodeWorld
import           Data.Text            (Text)
import           EscapeTheRoom.Levels

run :: IO ()
run = drawingOf blank
