{-# LANGUAGE OverloadedStrings #-}
-- | Levels for Escape the Room.
module EscapeTheRoom.Levels where

import qualified CodeWorld
import           Data.Text (Text)

-- | Tiles used in Escape the Room game.
data Tile
  -- standard tiles
  = Wall              -- ^ An unpassable wall.
  | Floor             -- ^ Floor to walk on.
  | Door DoorColor    -- ^ Door of a given color.
  | Button DoorColor  -- ^ A button that opens/toggles
                      -- all doors of the same color.
  | Exit              -- ^ An exit.
  -- extra tile types
  | Trap Direction    -- ^ A trap tile. Once character steps on this tile
                      -- a wall appears next to them.
  | Void              -- ^ A void tile, something to put outside level map.

data Direction = DirUp | DirDown | DirLeft | DirRight

-- | Available door and button colors.
data DoorColor
  -- standard colors
  = Red | Green | Blue
  -- extra colors
  | Pink | Purple | Yellow | Cyan | Orange
  | White | Gray | Black | Brown
  | DarkRed | DarkGreen
  | LightBlue | LightYellow
  deriving (Eq)

-- | Convert door color to CodeWorld's color.
doorColor :: DoorColor -> CodeWorld.Color
doorColor dc =
  case dc of
    Red         -> CodeWorld.red
    Blue        -> CodeWorld.blue
    Green       -> CodeWorld.green
    Purple      -> CodeWorld.purple
    Yellow      -> CodeWorld.brown
    Pink        -> CodeWorld.pink
    Brown       -> CodeWorld.brown
    Orange      -> CodeWorld.orange
    Cyan        -> CodeWorld.RGB 0.0 1.0 1.0
    White       -> CodeWorld.white
    Gray        -> CodeWorld.gray
    Black       -> CodeWorld.black
    DarkRed     -> CodeWorld.dark CodeWorld.red
    DarkGreen   -> CodeWorld.dark CodeWorld.green
    LightBlue   -> CodeWorld.light CodeWorld.blue
    LightYellow -> CodeWorld.light CodeWorld.yellow


-- | Coordinates on a level map.
data Coords = Coords Int Int

-- | Coordinates on a level map as a tuple.
type TupleCoords = (Int, Int)

toTupleCoords :: Coords -> TupleCoords
toTupleCoords (Coords i j) = (i, j)

-- | Level author name.
type Author = Text

-- | A level map with initial coordinates.
data Level = Level
  Author            -- Author of the level
  Coords            -- Start coordinates of the player.
  (Coords -> Tile)  -- Level map.
  [DoorColor]       -- Doors, opened on start.

-- | A list of all level maps.
allLevels :: [Level]
allLevels =
  [ level1
  , level2
  , level3
  , level4
  , level5
  , level6
  , level7
  , level8
  , level9_1
  , level9_2
  , level9_3
  , level10
  , level11
  , level12
  , level13
  , level14
  , level15
  , level16
  , level17
  , level18
  , level19
  ]

-- | Author: Kamil Akhmetov
level1 :: Level
level1 = Level "Kamil Akhmetov"
  (Coords 2 8) levelMap []
  where
    levelMap (Coords x y)
      -- borders
      | x < 1 || x > 19= Wall
      | y < 1 || y > 19 = Wall
      | (x+y) < 5 = Wall
      | (20-x+y) < 5 = Wall
      | (x+20-y) < 5 = Wall
      | (20-x+20-y) < 5 = Wall
      -- first layer
      | (x+y) > 7  && (x+y) <11 && x>2 &&y>2= Wall
      | (20-x+y) > 7  && (20-x+y) <11 && (20-x)>2 &&y>2= Wall
      | (x+20-y) > 7  && (x+20-y) <11 && x>2 &&(20-y)>2= Wall
      | (20-x+20-y) > 7  && (20-x+20-y) <11 && (20-x)>2 &&(20-y)>2= Wall
      | x == 3 && abs(10-y) <4 = Door Green
      | x == 20-3 && abs(10-y) <4= Door Blue
      | y == 3 && abs(10-x) <3 = Door Red
      | y == 20-3 && abs(10-x) <3 = Door Purple
      -- second layer
      | (x+y) > 13  && (x+y) <16 && x>5 &&y>5= Wall
      | (20-x+y) > 13  && (20-x+y) <16 && (20-x)>5 &&y>5= Wall
      | (x+20-y) > 13  && (x+20-y) <16 && x>5 &&(20-y)>5= Wall
      | (20-x+20-y) > 13  && (20-x+20-y) <16 && (20-x)>5 &&(20-y)>5= Wall
      | x == 14 && y==10 = Door Green
      | x == 6  && y==10 = Door Blue
      | x == 10 && y==6 = Door Red
      | x == 10 && y ==14 = Door Purple
      -- buttons
      | x == 10 && y ==1 = Button Red
      | x == 10 && y ==19 = Button Purple
      | x == 1 && y ==10 = Button Blue
      | x == 19 && y ==10 = Button Green
      --
      | abs (10-x) < 2  && abs (10-y) < 2 = Exit
      | otherwise              = Floor

-- | Author: Viacheslav Goreev
level2 :: Level
level2 = Level "Viacheslav Goreev"
  (Coords (-8) (-2)) (hardLevel . toTupleCoords) []
  where
    hardLevel :: TupleCoords -> Tile
    hardLevel (0, 0) = Exit
    --
    hardLevel (-6, -6) = (Button Blue)
    hardLevel (0, -8) = (Door Blue)
    hardLevel (3, 1) = (Door Blue)
    --
    hardLevel (8, -2) = (Button Red)
    hardLevel (-8, 0) = (Door Red)
    --
    hardLevel (-2, 8) = (Button Orange)
    hardLevel (0, 5) = (Door Orange)
    hardLevel (8, 0) = (Door Orange)
    --
    hardLevel (8, 2) = (Button Purple)
    hardLevel (0, 8) = (Door Purple)
    hardLevel (-4, 5) = (Door Purple)
    --
    hardLevel (x, y)
      | abs x > 10 || abs y > 10 = Wall
      | radius < 3 = Floor
      | x == 0 || y == 0 = Wall
      | (radius > 3 && radius < 4) || (radius > 6 && radius < 7) || radius >= 10 = Wall
      | radius < 10 = Floor
      | otherwise = Void
        where
          radius :: Double
          radius = sqrt((fromIntegral x)^2 + (fromIntegral y)^2)

-- | Author: Nikita Kostenko
level3 :: Level
level3 = Level "Nikita Kostenko"
  (Coords (-9) (-9)) levelMap []
  where
    levelMap :: Coords -> Tile
    levelMap (Coords (-10) _) = Wall
    levelMap (Coords 10 _)    = Wall
    levelMap (Coords _ (-10)) = Wall
    levelMap (Coords _ 10)    = Wall
    levelMap (Coords (-9) 9)  = Button Blue
    levelMap (Coords (-9) 8)  = Door Red
    levelMap (Coords (-9) 7)  = Button Green
    levelMap (Coords 9 9)     = Exit
    levelMap (Coords 9 8)     = Door Blue
    levelMap (Coords 9 7)     = Button Red
    levelMap (Coords 9 6)     = Door Green
    levelMap (Coords x y)     | x `mod` 4 == 2 && y < 9 = Wall
    levelMap (Coords x y)     | x `mod` 4 == 0 && y > (-9) = Wall
    levelMap _                = Floor

-- | Author: Kamil Alimov
level4 :: Level
level4 = Level "Kamil Alimov"
  (Coords (-1) 7) newLevelMap []
  where
    newLevelMap :: Coords -> Tile
    newLevelMap (Coords i j)
     | (i, j) == (-5, 2) = Door Red
     | (i, j) == (5, 2) = Door Purple
     | (i, j) == (-5, 3) = Button Blue
     | (i, j) == (5, 3) = Button Red
      | abs i > 9 || abs j > 9
        || abs i == 6 && j > 1 && j < 5
        || abs i == 4 && j > 1 && j < 5
        || abs i == 5 && j == 4
       -- || abs i < 6 &&  abs i > 2 && j > 1 && j < 5 &&
        ||  (i == -2 || i == 2) && j == -4
         ||  (i == -1 || i == 1) && j == -5
        || abs i < 5 && abs i > -1 && j < -1 && j > -4 = Wall
      | (i, j) == (0, -6) = Door Blue
      | (i, j) == (-4, 3) = Door Blue
      | (i, j) == (0, 1) = Button Purple
      | (i, j) == (0, -4) = Exit
      | otherwise = Floor

level5 :: Level
level5 = Level "Vyacheslav Vasilev"
  (Coords (-1) 7) (hwLevelMap . toTupleCoords) []
  where
    -- | A specific map for homework
    hwLevelMap (x, 1)
      | x > 5 && x < 15 = Floor
      | otherwise = Wall
    hwLevelMap (x, 2)
      | x > 3 && x < 8 = Floor
      | x > 10 && x < 13 = Door Blue
      | x > 12 && x < 17 = Floor
      | otherwise = Wall
    hwLevelMap (x, 3)
      | x > 2 && x < 6 = Floor
      | x > 7 && x < 10 = Floor
      | x > 10 && x < 13 = Floor
      | x > 12 && x < 15 = Door Blue
      | x > 14 && x < 18 = Floor
      | otherwise = Wall
    hwLevelMap (x, 4)
      | x > 1 && x < 4 = Floor
      | x > 5 && x < 10 = Floor
      | x == 10 = Door Blue
      | x > 10 && x < 13 = Button Green
      | x > 12 && x < 15 = Floor
      | x > 14 && x < 17 = Door Blue
      | x > 16 && x < 19 = Floor
      | otherwise = Wall
    hwLevelMap (x, 5)
      | x > 1 && x < 4 = Floor
      | x > 4 && x < 8 = Floor
      | x > 7 && x < 10 = Door Green
      | x > 12 && x < 15 = Button Green
      | x == 15 = Floor
      | x == 16 = Door Blue
      | x > 16 && x < 19 = Floor
      | otherwise = Wall
    hwLevelMap (x, 6)
      | x > 0 && x < 3 = Floor
      | x > 3 && x < 7 = Floor
      | x == 7 = Door Green
      | x > 7 && x < 10 = Floor
      | x > 10 && x < 13 = Floor
      | x > 13 && x < 16 = Button Green
      | x == 16 = Floor
      | x == 17 = Door Blue
      | x > 17 && x < 20 = Floor
      | otherwise = Wall
    hwLevelMap (x, 7)
      | x > 0 && x < 3 = Floor
      | x > 3 && x < 6 = Floor
      | x == 6 = Door Green
      | x > 6 && x < 9 = Floor
      | x == 9 = Button Red
      | x > 10 && x < 14 = Floor
      | x == 14 = Door Red
      | x == 15 = Button Green
      | x == 16 = Floor
      | x == 17 = Door Blue
      | x > 17 && x < 20 = Floor
      | otherwise = Wall
    hwLevelMap (x, 8)
      | x == 1 = Floor
      | x > 2 && x < 5 = Floor
      | x == 5 = Door Green
      | x > 5 && x < 8 = Floor
      | x == 8 = Button Red
      | x == 11 = Door Red
      | x > 11 && x < 15 = Floor
      | x == 16 = Button Green
      | x == 17 = Floor
      | x == 18 = Door Blue
      | x == 19 = Floor
      | otherwise = Wall
    hwLevelMap (x, 9)
      | x == 1 = Button Blue
      | x > 2 && x < 5 = Floor
      | x == 5 = Door Green
      | x == 6 = Floor
      | x == 7 = Button Red
      | x > 8 && x < 12 = Exit
      | x == 12 = Door Red
      | x > 12 && x < 15 = Floor
      | x == 16 = Button Green
      | x == 17 = Floor
      | x == 18 = Door Blue
      | x == 19 = Button Blue
      | otherwise = Wall
    hwLevelMap (x, 10)
      | x == 1 = Door Green
      | x > 8 && x < 12 = Exit
      | x == 19 = Door Red
      | otherwise = Wall
    hwLevelMap (x, 11) = hwLevelMap (x, 9)
    hwLevelMap (x, 12) = hwLevelMap (x, 8)
    hwLevelMap (x, 13) = hwLevelMap (x, 7)
    hwLevelMap (x, 14) = hwLevelMap (x, 6)
    hwLevelMap (x, 15) = hwLevelMap (x, 5)
    hwLevelMap (x, 16) = hwLevelMap (x, 4)
    hwLevelMap (x, 17) = hwLevelMap (x, 3)
    hwLevelMap (x, 18) = hwLevelMap (x, 2)
    hwLevelMap (x, 19) = hwLevelMap (x, 1)
    hwLevelMap (x, 20) = hwLevelMap (x, 0)
    hwLevelMap _ = Wall


level6 :: Level
level6 = Level "Aliya Zagidullina"
  (Coords (-6) 9) level []
  where
    level :: Coords -> Tile
    level (Coords i j)
       | i==(-10) = Wall
       | i==10 = Wall
       | j==(-10) = Wall
       | j==10 = Wall
       | i == 0 && (j/=7 && j/=8 && j>0) = Wall
       | j == 0 &&  i>0 = Wall
       | j == 0 &&  i==0 = Door Red
       | i>(-9) && i<(-1) && (j==7 || j==8) = Wall
       | i>(-2) && i<1 && (j==7 || j==8) = Door Blue
       | i==(-1) && j==6 = Wall
       | (i==(-2) || i==(-3)) && (j==6 || j==5) = Door Red
       | i==(-1) && j==5 = Door Red
       | j<7 && j>3 && (i==(-8) || i==(-7) || i==(-6)) = Wall
       | i>(-9) && i<(-6) && (j>(-8) && j<2) = Wall
       | i==(-9) && (j>(-8) && j<2) = Door Green
       | i==(-6) && j==(-2) = Wall
       | i==(-6) && j==(-3) = Door Red
       | i==(-5) && j==(-3) = Wall
       | i==(-5) && j==(-4) =  Door Red
       | i==(-4) && j==(-4) = Wall
       | i==(-4) && j==(-5) = Door Red
       | i==(-3) && j==(-5) = Door Green
       | i==(-3) && j==(-6) = Door Red
       | i==(-2) && j==(-6) = Wall
       | i==(-2) && j==(-7) = Door Red
       | i==(-1) && j==(-7) = Wall
       | i==(-1) && j==(-8) = Door Red
       | i==(0) && j==(-8) = Wall
       | i==(0) && j==(-9) = Door Red
       | i==(0) && j==(-9) = Door Green
       | i==(-7) && j==(-8) = Button Red
       | i==(0) && j==(-1) = Button Green
       | i==1 && j==(-2) = Button Green
       | i==2 && j==(-3) = Button Green
       | i==3 && j==(-4) = Button Green
       | i==2 && j==(-5) = Button Green
       | i==1 && j==(-6) = Button Green
       | i==0 && j==(-7) = Button Green
       | i==4 && j<0 && j/=(-9) = Door Green
       | i==5 && j<0 && j/=(-9) = Wall
       | (i==4 || i==5) && j==(-9) = Door Red
       | i==9 && j==(-1) = Button Blue
       | i==9 && j==1 = Exit
       | otherwise = Floor

level7 :: Level
level7 = Level "Igor Vakhula"
  (Coords (-9) (-9)) (tileMap . toTupleCoords) []
  where
    -- | tile map defining
    tileMap :: TupleCoords -> Tile
    tileMap (-10, _) = Wall
    tileMap (_, -10) = Wall
    tileMap (10, _)  = Wall
    tileMap (_, 10)  = Wall
    -- buttons
    tileMap (-5, 9)  = Button Pink
    tileMap (-1, 9)  = Button Brown
    tileMap (3, 9)   = Button Blue
    -- doors
    tileMap (-4, -9) = Door Pink
    tileMap (0, -9)  = Door Brown
    tileMap (4, -9)  = Door Blue
    -- room's wall restrictions XD
    tileMap (-4, _)  = Wall
    tileMap (0, _)   = Wall
    tileMap (4, _)   = Wall
    -- exit
    tileMap (9, 0)   = Exit
    --
    tileMap _        = Floor

level8 :: Level
level8 = Level "Pavel Vybornov"
  (Coords 1 1) (customMap . toTupleCoords) []
  where
    -- | Used to set the tiles on the map
    customMap :: TupleCoords -> Tile
    customMap (i, j)
      | abs i == 10 || abs j == 10 || (abs i == 8 && abs j <= 8)
        || (abs i <= 8 && abs j == 8 && (not (i == 6 && j == 8)))
        || (abs i == 6 && abs j <= 6) || (abs i == 4 && abs j <= 4)
        || (abs i <= 6 && abs j == 6 && (not (i == 5 && j == 6)))
        || (abs i <= 4 && abs j == 4 && (not (i == 3 && j == 4)))
        || (abs i == 2 && abs j <= 2) || (abs i <= 2 && j == (-2))
        || (j == 9 && i == 9) || (j == 6 && i == 7)  || (j == 4 && i == 5)
        || (j == 2 && i == 3) || (j == 1 && i == 0) || (j == 0 && i == 0)
        || (j == 2 && i == 0) || (j == 2 && i == (-1)) = Wall
      | (i == (-1) && j == 1) = Button Red
      | (i == 3 && j == 1) = Button Green
      | (i == 5 && j == 3) = Button Blue
      | (i == 7 && j == 5) = Button Pink
      | (i == 9 && j == 8) = Button Purple
      | (i == 1 && j == 2) = Door Red
      | (i == 3 && j == 4) = Door Green
      | (i == 5 && j == 6) = Door Blue
      | (i == 6 && j == 8) = Door Pink
      | (i == 7 && j == 9) = Door Purple
      | j == 9 && i == 8 = Exit
      | otherwise = Floor

level9_1 :: Level
level9_1 = Level "Niyaz Fahretdinov"
  (Coords 0 0) (customMap . toTupleCoords) []
  where
    Map _ customMap = map2
    --
    -- | Second level of the game
    map2 :: Map
    map2 = Map ((-2, -2), (10, 4)) level
                    <-- boxOf (-1, -1) (9, 3) Wall
                    <-- boxOf (-2, -2) (10, 4) Floor
      where
        level (8, _) = Exit
        --
        level (5, _) = Button Green
        level (7, _) = Door Green
        --
        level (1, 0) = Wall
        level (1, 1) = Wall
        --
        level (3, 2) = Wall
        level (3, 1) = Wall
        --
        level _      = Floor

level9_2 :: Level
level9_2 = Level "Niyaz Fahretdinov"
  (Coords 0 0) (customMap . toTupleCoords) []
  where
    Map _ customMap = map3
    -- | Third level of the game
    map3 :: Map
    map3 = emptyMap (-2, -14) (14, 2)
                <-- boxOf (-1, -13) (13, 1)           Wall
                --
                <-- xLineOf 1 (-11, -3)   Wall
                <-- xLineOf 3 (0,   -1)   Wall
                <-- xLineOf 3 (-9,  -11)  Wall
                <-- xLineOf 3 (-7,  -5)   Wall
                --
                <-- yLineOf (-1) (1, 3)   Wall
                <-- yLineOf (-3) (1, 5)   Wall
                <-- tileAt (2, -11)       Wall
                --
                <-- xLineOf 5 (-12, -9)   Wall
                <-- xLineOf 5 (-7,  -1)   Wall
                --
                <-- yLineOf (-1)  (5, 7)  Wall
                <-- yLineOf (-7)  (3, 7)  Wall
                <-- yLineOf (-11) (5, 9)  Wall
                --
                <-- xLineOf 7 (-3, -5)    Wall
                <-- xLineOf 7 (-7, -9)    Wall
                --
                <-- yLineOf (-3) (7, 9)   Wall
                <-- yLineOf (-5) (7, 12)  Wall
                <-- yLineOf (-9) (7, 11)  Wall
                --
                <-- xLineOf 9 (-1, -3)    Wall
                <-- xLineOf 9 (-5, -7)    Wall
                <-- xLineOf 9 (-9, -11)   Wall
                <-- xLineOf 11 (0, -3)    Wall
                <-- xLineOf 11 (-7, -9)   Wall
                <-- xLineOf 11 (-11, -12) Wall
                --
                <-- tileAt (6, -6)        (Button Green)
                <-- tileAt (2, -10)       (Button Green)
                <-- tileAt (12, -11)      (Door Green)
                --
                <-- tileAt (8, -10)       (Button Yellow)
                <-- tileAt (2, -9)        (Door Yellow)
                <-- tileAt (2, -7)        (Door Yellow)
                --
                <-- tileAt (4, -6)        (Button Blue)
                <-- tileAt (8, -1)        (Door Blue)
                --
                <-- tileAt (12, -12)      Exit

level9_3 :: Level
level9_3 = Level "Niyaz Fahretdinov"
  (Coords 0 0) (customMap . toTupleCoords) [Red]
  where
    Map _ customMap = map4
    -- | 4th level of the game
    map4 :: Map
    map4 = moveOrigin (0, -4) $ Map ((-10, -10), (10, 10)) level
                <-- boxOf (-10, -10) (10, 10)  Exit
                <-- boxOf (-9, -9) (9, 9)      (Door Yellow)
                <-- boxOf (-8, -8) (8, 8)      (Door Green)
                <-- boxOf (-7, -7) (7, 7)      (Door Blue)
                <-- boxOf (-6, -6) (6, 6)      (Door Red)
                --
                <-- boxOf (-5, -5) (5, 5)      Wall
                <-- boxOf (-4, -4) (4, 4)      Wall
                <-- boxOf (-3, -3) (3, 3)      Wall
                --
                <-- xLineOf 0 (3, 5)           Floor
                <-- xLineOf 0 (-3, -5)         Floor
                --
                <-- yLineOf 0 (3, 5)           Floor
                <-- yLineOf 0 (-3, -5)         Floor
      where
        level (x, y)
            | abs x == 2 && abs y == 2 =  Button Green
            | abs x + abs y < 2        =  Button Yellow
            | (x + y) `mod` 2 == 0     =  Button Red
            | otherwise                =  Button Blue

level10 :: Level
level10 = Level "Alexey Zhuchkov"
  (Coords 10 10) level []
  where
    -- | Map of the game
    level :: Coords -> Tile
    level (Coords x y)
      | (x, y) == (20, 10) = Exit
      | (x, y) == (1, 1) = Button Green
      | (x, y) == (18, 10) || (x, y) == (3, 1) || (x, y) == (6, 10) = Door Green
      | (x, y) == (19, 16) = Button Purple
      | (x, y) == (12, 7) = Door Purple
      | (x, y) == (15, 16) = Button Pink
      | (x, y) == (20, 2) = Door Pink
      | (x, y) == (11, 16) = Button Brown
      | (x, y) == (15, 14) = Door Brown
      | (x, y) == (19, 19) = Button White
      | (x, y) == (19, 14) = Door White
      | (x, y) == (18, 7) = Button Red
      | (x, y) == (11, 14) ||  (x, y) == (13, 8) = Door Red
      | x >= 0 && x <= 21 && (y == 0 || y == 21) = Wall
      | y >= 0 && y <= 21 && (x == 0 || x == 21) = Wall
      | y >= 2 && y <= 19 && (x == 3) = Wall
      | y >= 2 && y <= 21 && (x == 6) = Wall
      | x >= 6 && x <= 21 && (y == 2) = Wall
      | x >= 8 && x <= 19 && (y == 4) = Wall
      | x >= 7 && x <= 11 && (y == 6) = Wall
      | (x == 15 || x == 11) && y == 7 = Wall
      | x >= 15 && x <= 21 && (y == 6)= Wall
      | x >= 9 && x <= 21 && (y == 9 || y == 11 || y == 14 || y == 17) = Wall
      | y >= 14 && y <= 17 && (x == 9 || x == 13 || x == 17 ) = Wall
      | y >= 4 && y <= 7 && (x == 13) = Wall
      |otherwise = Floor

level11 :: Level
level11 = Level "Muhammad Mavlyutov"
  (Coords 1 1) (levelMap1 . toTupleCoords) []
  where
    -- | First level
    levelMap1 :: TupleCoords -> Tile
    levelMap1 (x, y)
      | x == 0 || x == 21 || y == 0 || y == 21 = Wall
      | (x, y) == (20, 20) = Exit
      | checkRadius (0,0) (1) = Button Purple
      | checkRadius (0,0) (2) = Button Blue
      | checkRadius (0,0) (3) = Button LightBlue
      | checkRadius (0,0) (4) = Button Green
      | checkRadius (0,0) (5) = Button LightYellow
      | checkRadius (0,0) (6) = Button Orange
      | checkRadius (0,0) (7) = Button Red
      | checkRadius (0,0) (8) ||
        checkRadius (21,21) (8) = Door Red
      | checkRadius (0,0) (9) ||
        checkRadius (21,21) (9) = Door Orange
      | checkRadius (0,0) (10) ||
        checkRadius (21,21) (10) = Door LightYellow
      | checkRadius (0,0) (11) ||
        checkRadius (21,21) (11) = Door Green
      | checkRadius (0,0) (12) ||
        checkRadius (21,21) (12) = Door LightBlue
      | checkRadius (0,0) (13) ||
        checkRadius (21,21) (13) = Door Blue
      | checkRadius (0,0) (14) ||
        checkRadius (21,21) (14) = Door Purple
      | otherwise = Floor
     where
      checkRadius :: (Int, Int) -> Int -> Bool
      checkRadius (cx, cy) r = floor(sqrt (xs2 + ys2)) == r
        where
          xs2 = fromIntegral ((abs(cx-x))^2)
          ys2 = fromIntegral ((abs(cy-y))^2)

level12 :: Level
level12 = Level "Arina Fedorovskaya"
  (Coords 8 (-9)) level []
  where
    level :: Coords -> Tile
    -- | Frame for map
    level (Coords (-10) _) = Wall
    level (Coords 10 _) = Wall
    level (Coords _ (-10)) = Wall
    level (Coords _ 10) = Wall
    -- | Walls
    level (Coords x y)
     | (y == (-5) && x >= 7) || y == -4 && x == 1
     || (y < (-6) && y > (-8)) && x == 1
     || ((x > 6 || x < 6) && (y == -8))
     || ((y == -3) && (x < -8 || x > -5))
     || ((x == 7) &&  (y == (-6)))
     || (x == -3) && (y < -3 && y > -7)
     || (y == 0) && x < 7
     || (y == -1 && x == 6)
     || (x == 5 || x == 4) && (y < 10 && y > 6)
     || (x < 10 && x > 7) && y == 5
     || (x < 7 && x > 3) && y == 5
     || (y < 9 && y > 1) && (x == -7 || x == -1)
     || (y < 8 && y > 0) && (x == -4 || x == 2) = Wall
    -- | Buttons
    level (Coords (-8) (-9)) = Button Red
    level (Coords 9 (-6)) = Button Red
    level (Coords x y)
      | ((x == 3 || x == 4 || x == 5) && (y == -4)) = Button Blue
    level (Coords x y)
      | (x == -9) && (y < -3 && y > -8) = Button Pink
    level (Coords x y)
      | (x == -9) && (y < 0 && y > -3) = Button Orange
    level (Coords (-9) 9) = Button Green
    -- | Doors
    level (Coords x y)
      | ((x == 1) && (y == -5 || y == -6)) = Door Red
    level (Coords x y)
      | (x > -9 && x < -4) && (y == -3) = Door Blue
    level (Coords x y)
      | (x < 10 && x > 6) && (y == 0) = Door Orange
    level (Coords 5 6) = Door Pink
    level (Coords 4 6) = Door Green
    level (Coords x y)
      | (x > -8 && x < 6) && (y == 9) = Door Green
    -- | Traps
    level (Coords 6 (-7)) = Trap DirDown
    level (Coords 8 (-7)) = Trap DirLeft
    level (Coords (-4) (-7)) = Trap DirRight
    level (Coords 7 (-2)) = Trap DirLeft
    level (Coords 7 4) = Trap DirUp
    level (Coords 1 8) = Trap DirRight
    level (Coords (-5) 8) = Trap DirRight
    level (Coords (-2) 1) = Trap DirRight
    level (Coords (-8) 1) = Trap DirRight
    -- | Exit
    level (Coords x y)
     | (x == 8 || x == 7) && (y == 8 || y == 7) = Exit
    -- | Floor
    level _ = Floor

level13 :: Level
level13 = Level "Vladislav Savchuk"
  (Coords 1 1) (myLevel . toTupleCoords) []
  where
    -- | A level specification. Given a coordinate returns the tile on this coordinate
    -- This map is of size 21x21 from (0,0 to (20, 20)) (inclusive)
    myLevel :: TupleCoords -> Tile
    myLevel (x, y)
      | y == 8 && x == 11 = Exit
      | (y == 1) && x > 3 && x < 17 = Door Red
      | (y == 2) && x == 3 = Button Red
      | (x == 2 && y == 9)
        || (x == 3 && y == 8)
        || (x == 17 && y == 8)
        || (x == 18 && y == 9) = Door Blue -- cheeks
      | (y == 2) && x == 17 = Button Blue
      | (x == 7 || x == 14) && (y == 13) = Button Orange
      | (y > 6 && y < 10 && x > 9 && x < 13) = Door Orange -- nose
      | ((x == 12 || x == 16 || x == 5 || x == 9) && y > 11 && y < 15)
        || ((y==11 || y == 15) && ((x > 5 && x < 9)||(x > 12 && x < 16))) = Door Gray -- eyes
      | ((x == 10 || x == 11 ) && y == 4) = Button Gray
      | (((x > 12 && x < 16) || (x > 5 && x < 9)) && y > 11 && y < 15)  = Door White
      | (x == 0)
        || (x == 20)
        || (y == 0)
        || (y == 20)
        || (y == 2 && x > 3 && x < 17)
        || (y == 19 && x > 4 && x < 16)
        || ((x == 1 || x == 19) && y > 9 && y < 14)
        || ((x == 2 || x == 18) && ((y < 17 && y > 13) || (y == 9)))
        || ((x == 3 || x == 17) && ((y < 19 && y > 16) || (y == 8)))
        || ((x == 4 || x == 16) && ((y > 2 && y < 8) || (y==18)))
        || (y > 2 && y < 6 && (x == 8 || x == 13)) = Wall
      | (x >= 5 && x <= 16) && (y>=3 && y<=5) = Button White -- teeth
      | otherwise = Floor

level14 :: Level
level14 = Level "Maxim Salo"
  (Coords 2 2) worldMap []
  where
    -- |
    -- | World
    -- |
    worldMap :: Coords -> Tile
    worldMap (Coords i j)
      -- | Walls around the map
      | i == 1 || j == 1 || i == 21 || j == 21 = Wall
      -- | Second door and button
      | i == 10 && j == 11 = Door Green
      | i == 14 && j == 4 = Button Green
      -- | Third door and button
      | i == 20 && j == 10 = Button Purple
      | i == 11 && j == 20 = Door Purple
      -- | World separation walls
      | (i + 1 == j && j /= 20) || 31 - i == j = Wall
      -- | Blue button
      | i == 2 && j == 10 = Button Blue
      -- | Red buttons
      | (i == 20 && j == 9)
        || (i == 11 && j == 11)
        || (i == 10 && j == 10)
        || (i == 19 && j == 10)
          = Button Red
      -- | Exit with its walls and door
      | i == 20 && j == 20 = Exit
      | i == 19 && j == 20 = Door Red
      | i == 18 && j == 20 = Door Blue
      | otherwise = Floor

level15 :: Level
level15 = Level "Arina Kuznetsova"
  (Coords 6 6) (initialLevelMap . toTupleCoords) []
  where
    -- | An initial specific map of a new room 21x21
    initialLevelMap :: TupleCoords -> Tile
    initialLevelMap (i, j)
      | i < 0 || i > 20 || j < 0 || j > 20 = Void
      | (i == 11) && (j== 10) = Button Red
      | (i == 2) && j == 3 = Button Blue
      | i == 5 && j == 3 = Door Red
      | i == 14 && j == 18 = Button Green
      | i == 3 && j == 17 = Door Green
      | i == 9 && j == 1 = Exit
      | i == 4 && j == 6 = Door Blue
      | (i == 0) || (i == 20) || (j == 0) || (j == 20)
      || i == 8 && j == 1
      || i == 10 && j == 15
      || i == 11 && j == 14
      || i == 12 && j == 13
      || j == 12 && (i >= 13 && i <= 16)
      || (((j >= 6 && j <= 8) || j == 14) && i == 8)
      || (((j >= 8 && j <= 10) || j == 13) && i == 9)
      || ((j >= 10 && j <= 12) && i == 10)
      || (j == 11 && i == 11)
      || ((j == 9  || j == 10) && i == 12)
      || ((j >= 7  && j <= 9) && i == 13)
      || ((j >= 6  && j <= 7) && i == 14)
      || (i == 5) && (j <= 5)
      || j == 5 && i <= 5
      || i == 2 && (j >= 7 && j <= 18)
      || i == 2 && j == 2
      || i == 3 && (j == 2 || j == 3)
      || i == 12 && (j >= 16 && j <= 18)
      || i == 4 && (j >= 7 && j <= 16)
      || j == 18 && (i >= 2 && i <= 12)
      || j == 16 && ((i >= 4 && i <= 9) || (i >= 12 && i <= 16))
      || i == 16 && ((j >= 2 && j <= 12)||(j >= 16 && j <= 20))
      || j == 2 && (i >= 8 && i <= 16) = Wall
      | otherwise = Floor

level16 :: Level
level16 = Level "Patrusheva Elena"
  (Coords 0 0) myLevelMap []
  where
    -- | A specific map of a level definition
    myLevelMap :: Coords -> Tile
    myLevelMap (Coords 0 0) = Floor
    myLevelMap (Coords 0 1) = Floor
    myLevelMap (Coords 1 1) = Button Green
    myLevelMap (Coords 1 0) = Button Orange
    myLevelMap (Coords 2 2) = Door Orange
    myLevelMap (Coords 11 1) = Button Purple
    myLevelMap (Coords 13 6) = Door Purple
    myLevelMap (Coords 16 16) = Button Pink
    myLevelMap (Coords 17 20) = Door Pink
    myLevelMap (Coords 18 20) = Door Pink
    myLevelMap (Coords 3 3) = Button Red
    myLevelMap (Coords 19 19) = Exit
    myLevelMap (Coords 7 6) = Button Blue
    myLevelMap (Coords x 5)
      | not (inRange x (0, 20)) = Wall
      | x `mod` 5 == 0 = Door Red
      | x `mod` 5 == 1 = Door Blue
      | x > 14 = Floor
      | otherwise = Wall
    myLevelMap (Coords x y)
      | not (inRange x (0, 20)) || not (inRange y (0, 20)) = Wall
      | x == y + 2 && x < 13 = Door Blue
      | x == 17 = Wall
      | x == 15 = Door Green
      | x == 13 = Wall
      | y == 2 = Wall
      | y == 18 = Wall
      | y == 7 = Wall
      | y == 14 = Wall
      | x == y - 3 || x == y-2 = Wall
      | x == y + 13 = Wall
      | otherwise = Floor

    -- | Checking if the value in defined range
    -- * 'x' - value
    -- * 'l' - lover bound
    -- * 'u' - upper bound
    inRange :: Int -> (Int, Int) -> Bool
    inRange x (l, u) = x >= l && x <= u

level17 :: Level
level17 = Level "Rim Rakhimov"
  (Coords 0 0) tileAt2 []
  where
    -- | Tile at the given coordinate (defines the map for the task 2)
    tileAt2 :: Coords -> Tile
    -- exits
    tileAt2 (Coords 9 (-1)) = Exit
    tileAt2 (Coords 8 (-1)) = Exit
    -- doors
    tileAt2 (Coords 6 (-1)) = Door Red
    tileAt2 (Coords 6 (-8)) = Door Blue
    tileAt2 (Coords 5 3) = Door Green
    tileAt2 (Coords (-5) (-5)) = Door Gray
    tileAt2 (Coords (-4) (-6)) = Door Gray
    tileAt2 (Coords (-3) (-7)) = Door Gray
    tileAt2 (Coords (-2) (-8)) = Door Gray
    tileAt2 (Coords (-1) (-9)) = Door Gray
    tileAt2 (Coords 0 (-6)) = Door Orange
    -- buttons
    tileAt2 (Coords 5 (-8)) = Button Red
    tileAt2 (Coords 7 3) = Button Blue
    tileAt2 (Coords (-5) (-8)) = Button Blue
    tileAt2 (Coords (-4) 0) = Button Green
    tileAt2 (Coords (-8) 7) = Button Gray
    tileAt2 (Coords (-4) (-7)) = Button Gray
    tileAt2 (Coords 5 2) = Button Orange
    -- floors
    tileAt2 (Coords i j)
      | j == -9 && (i >= -8 && i <= -3 || i >= 1 && i <= 4 || i >= 6 && i <= 9) = Floor
      | j == -8 && (i == -9 || i >= -7 && i <= -1 || i >= 1 && i <= 3 || i >= 5 && i <= 9) = Floor
      | j == -7 && (i >= -9 && i <= -8 || i >= -6 && i <= 0 || i >= 2 && i <= 4 || i >= 7 && i <= 9) = Floor
      | j == -6 && (i >= -9 && i <= -7 || i >= -5 && i <= 9) = Floor
      | j == -5 && (i >= -9 && i <= -2 || i >= 0 && i <= 1 || i >= 3 && i <= 9) = Floor
      | j == -4 && (i == -9 || i == -7 || i == 1 || i == 3 || i >= 5 && i <= 9) = Floor
      | j == -3 && (i == -9 || i >= -7 && i <= -1 || i == 1 || i == 3 || i >= 5 && i <= 9) = Floor
      | j == -2 && (i == -9 || i == -1 || i == 1 || i == 3) = Floor
      | j == -1 && (i == -9 || i >= -7 && i <= -1 || i == 1 || i >= 3 && i <= 9) = Floor
      | j == 0 && (i == -9 || i >= -7 && i <= -2 || i >= 0 && i <= 1) = Floor
      | j == 1 && (i == -9 || i >= -7 && i <= -1 || i >= 1 && i <= 3 || i >= 5 && i <= 9) = Floor
      | j == 2 && (i >= -3 && i <= -1 || i == 3 || i == 9) = Floor
      | j == 3 && (i >= -9 && i <= -5 || i >= -3 && i <= 1 || i >= 3 && i <= 7 || i == 9) = Floor
      | j == 4 && (i == -7 || i == -5 || i == -3 || i == 3 || i == 7 || i == 9) = Floor
      | j == 5 && (i >= -9 && i <= -7 || i == -5 || i >= -3 && i <= 1 || i >= 3 && i <= 5 || i == 7 || i == 9) = Floor
      | j == 6 && (i >= -9 && i <= -4 || i == 3 || i == 5 || i == 7 || i == 9) = Floor
      | j == 7 && (i >= -9 && i <= -7 || i == -4 || i >= -2 && i <= 3 || i == 5 || i == 7 || i == 9) = Floor
      | j == 8 && (i >= -9 && i <= -7 || i == -4 || i == 3 || i == 5 || i == 9) = Floor
      | j == 9 && (i >= -9 && i <= -7 || i >= -4 && i <= 3 || i >= 5 && i <= 9) = Floor
    -- walls
    tileAt2 _ = Wall

level18 :: Level
level18 = Level "Mikhail Tkachenko"
  (Coords 1 1) (levelTwoMap . toTupleCoords) []
  where
    -- Map for solutions 2-4
    levelTwoMap :: TupleCoords -> Tile
    levelTwoMap (i, j)
      | (i, j) == (19, 1) = Exit
      | (i, j) == (3, 2) = Door Blue
      | (i, j) == (3, 18) = Door Green
      | (i, j) == (9, 5) = Door White
      | (i, j) == (worldSize-3, 6) = Door Red
      | (i, j) == (1, 18) = Button Red
      | (i, j) == (16, 1) = Button White
      | (i, j) == (2, 1) = Button Blue
      | (i, j) == (19, 19) = Button Green
      | (i, j) == (19, 5) = Floor
      | (i == 0 || j == 0 || i == worldSize || j == worldSize) = Wall
      | ((i >= worldSize - 3 && j == worldSize - 4)
          || (j <worldSize -3 && i == worldSize -3)) = Wall
      | ((i <= 3 && j == 3) || (j <3 && i == 3)) = Wall
      | (j == 5) = Wall
      | (i == 3) = Wall
      | (i == 10 && j > 5 && j < 19) = Wall
      | (j == 18) = Floor
      | (i < 5 && j > 15) = Wall
      | otherwise = Floor

    -- Map size (only 20x20 fits the canvas)
    worldSize:: Int
    worldSize = 20

level19 :: Level
level19 = Level "Bykov Artemii"
  (Coords (-worldSize + 1) (-worldSize + 1)) (complexMap . toTupleCoords) []
  where
    -- | Global world size. For worldSize=8 coords should be between -8 and 8.
    worldSize :: Int
    worldSize = 8

    -- | Complex map for further solutions.
    complexMap :: TupleCoords -> Tile
    complexMap (x, y)
    -- world-bounding tiles
      | x == worldSize + 1 || x == -worldSize || y == worldSize + 1 || y == -worldSize = Wall
    -- actual world tiles
      | (x, y) == (3, 3) = Exit
      | x == 2 && x > -worldSize && x < worldSize || y == 2 = Door Green
      | (x, y) == (4, 4) = Button Green
      | (x, y) == (1, 1) = Button Green
      | (x, y) == (-6, -6) = Button Blue
      | (x, y) == (-7, -6) || (x, y) == (-7, -5) || (x, y) == (-6, -5) ||
        (x, y) == (-5, -5) || (x, y) == (-5, -6) || (x, y) == (-5, -7)
        = Door Blue
    -- catch-all for floor tiles
      | x > -worldSize && x < worldSize + 1 && y > -worldSize && y < worldSize + 1 = Floor
    -- catch-all for out-of-world tiles
      | otherwise        = Void

--

-- | Format for the game maps.
-- Bounds for the level
-- and the functions that gives tile for every coordinate
data Map = Map Bounds (TupleCoords -> Tile)

type Bounds = ((Int, Int), (Int, Int))

-- | Creates a bounded by the coordinates map made of only Floor
emptyMap :: TupleCoords -> TupleCoords -> Map
emptyMap start end = Map (start, end) (const Floor)

-- | An map operation that helps mutate maps
-- it's a function from TupleCoords to Maube Tile
-- when it is Nothing, the level is not changed at this coords
-- when it is Just someTile, the level tile changes to someTile at the coords
data MapOperation = Tiling (TupleCoords -> Maybe Tile)

-- | An operation for applying the peration to a map
(<--) :: Map -> MapOperation -> Map
(Map bounds level) <-- (Tiling f) = Map bounds newLevel
  where
    newLevel coords =
      case f coords of Nothing   -> level coords
                       Just tile -> tile

-- | An operation that draws a (x = a) line bounded at [sy; ey]
xLineOf :: Int -> TupleCoords -> Tile -> MapOperation
xLineOf a (sy, ey) tile = Tiling lined
  where
    bot = min sy ey
    top = max sy ey

    lined (i, j)
      | i == a && j >= bot && j <= top = Just tile
      | otherwise                    = Nothing

-- | An operation that draws a (y = a) line bounded at [sx; ex]
yLineOf :: Int -> TupleCoords -> Tile -> MapOperation
yLineOf y xBounds tile = Tiling (\(i, j) -> xOp (j, i))
  where
    Tiling xOp = xLineOf y xBounds tile

-- | An operation that changes tile at (x, y) to specified one
tileAt :: TupleCoords -> Tile -> MapOperation
tileAt (x, y) tile = Tiling point
  where
    point (i, j)
      | i == x && j == y = Just tile
      | otherwise        = Nothing

-- | An operation thet draw an empty suare
boxOf :: TupleCoords -> TupleCoords -> Tile -> MapOperation
boxOf (sx, sy) (ex, ey) tile = Tiling box
  where
    l = min sx ex
    r = max sx ex
    b = min sy ey
    t = max sy ey

    box (i, j)
      | (i == l || i == r) && j >= b && j <= t = Just tile
      | (j == b || j == t) && i >= l && i <= r = Just tile
      | otherwise                            = Nothing


-- | An operation that keeps the level and the bounds the same
-- But moved by (x, y)
moveOrigin :: TupleCoords -> Map -> Map
moveOrigin (x, y) (Map (oldStart, oldEnd) level) = Map newBounds newLevel
  where
    newBounds = ( (fst oldStart + x, snd oldStart + y)
                , (fst oldEnd + x, snd oldEnd + y)
                )
    newLevel (i, j) = level (i - x, j - y)
