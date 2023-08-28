{-# LANGUAGE RecordWildCards #-}

module Initialize where
import Lib (Ball(..), World(..), Paddle(..), Level(..), Bonus(..), Brick(..), State(..), EndReason(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.Gloss.Data.Color (Color, white, red, orange, violet, green, azure, yellow, makeColorI, magenta)
import Graphics.Gloss.Data.Picture (Picture)

{-
  Contains definitions for the initial 
  states of World, Level, Ball. 
-}



-- Useful constants for collision checking
-- and aligning screen elements
left, right, top, bottom :: Float
left = - fromIntegral width / 2
right = fromIntegral width / 2
top = fromIntegral height / 2
bottom = - fromIntegral height / 2

width, height :: Int
width = 1200
height = 800

-- number of seconds to clear each level
levelTimer :: Float
levelTimer = 200


haskellPurple1, haskellPurple2, haskellPurple3 :: Color
haskellPurple1 = makeColorI 69 59 96 255
haskellPurple2 = makeColorI 95 82 135 255
haskellPurple3 = makeColorI 141 80 139 255

-- Ball initially starts at the center falling
-- but is updated to the cursor's X position
initialBall :: Ball
initialBall = Ball { 
  radius = 12.0, 
  ballPosX = 0, 
  ballPosY = -260, 
  ballVelX = 0, 
  ballVelY = -7, 
  ballColour = white, 
  ballBonuses = []    -- start with no effects
}

-- Paddle is initially centered but also follows
-- cursor X position
initialPaddle :: Paddle
initialPaddle = Paddle { 
  paddlePosX = 0, 
  paddlePosY = -350, 
  paddleLength = 180, 
  paddleThickness = 15
}

-- This World is only created at the start of the game. When a new
-- game starts, `resetWorld` is applied to the current World
-- Takes a list of string representations of Levels, and loads them
-- into `[Level]`. We store a copy of loaded worlds under allLevels
-- so it can be reset without generating again. 
makeWorld :: [String] -> Picture -> Int -> World
makeWorld levelStrings logoImg savedHighscore = World {
    paddle                    = initialPaddle , -- paddle and ball at center X of window 
    balls                     = [initialBall] ,
    currentLevels             = levels        ,
    allLevels                 = levels        , -- a copy of all levels so we can replace currentLevels after a game ends
    state                     = Title         , -- Start with the title screen 
    score                     = 0             ,
    lives                     = 3             , -- Is reset after a game
    timer                     = levelTimer    , -- Seconds to complete the current level
    storedHighscore           = savedHighscore, -- When a game ends, store a new highscore here. On game launch, take value from highscore.txt 
    logoImg                   = logoImg       ,
    t                         = 0               -- ticks for animations
  } where levels = map toLevel levelStrings

-- | Resets world parameters like `makeWorld`, except
-- without needing to generate Levels again, like `makeWorld`
resetWorld :: World -> World
resetWorld w@World{..} = w {
    state                     = StartLevel, -- In this state, the game is paused, waiting for a SPACE key
    score                     = 0,
    paddle                    = initialPaddle, 
    balls                     = [initialBall],
    currentLevels             = allLevels,
    lives                     = 3,
    timer                     = levelTimer
  }

-- | Takes an `n` lines by `m` characters String and
-- generates a `Map (Int, Int) Brick` based on its characters.
toLevel :: String -> Level
toLevel levelText =
  Level {
    -- store rows and cols for converting to/from Brick Map indexes/screen coordinates
    levelRows      = rows,
    levelCols      = columns,
    -- Current level's brick dimensions: brick width X brick length
    levelBrickDims = ( fromIntegral width  / fromIntegral columns, 
                       fromIntegral height / fromIntegral rows    ),
    bricks         = generateBricks textRows rows columns -- create the Brick map using a level's String list
  }
  where
    -- assume all lines are the same length and are not empty
    textRows = lines levelText         -- split String by lines, 
    rows = length textRows             -- Each line is a row
    columns = (length . head) textRows -- Get the length of the first row (unsafely since we need not handle the error case)

-- | Unsafely converts a list of lines into
-- a Bricks Map where each key is an index pair (x, y)
-- where x is the column of a brick and y is its row.
generateBricks :: [String] -> Int -> Int -> Map (Int, Int) Brick
generateBricks textRows rows columns = 
  Map.fromList [ 
        ( (x, y), makeBrick $ (textRows !! y) !! x  )    -- unsafe indexing since the error case cannot be reasonably handled
         | y <- [0 .. rows - 1], x <- [0 .. columns - 1] -- inefficient indexing O(n^2 * m^2) but this is run only at game start up
    ]

  where
    -- Treat '-' as empty space
    -- Other characters create a coloured brick
    makeBrick :: Char -> Brick
    makeBrick textChar = case textChar of
      '-' -> Empty
      chr -> let (bonus, color, durability) = getBrickParams chr in
        Brick {
          brickColor         = color,
          brickDurability    = durability,
          brickMaxDurability = durability, -- store the original durability to calculate durability lost
          brickBonus         = bonus
        }

    -- Returns a tuple containing (possible bonus, colour, durability)
    getBrickParams :: Char -> (Maybe Bonus, Color, Int)
    getBrickParams c = case c of
      '*' -> (Just MultiBall        , white      , 2)
      '%' -> (Just LargePaddle      , white      , 2)
      '#' -> (Just IndestructiBall  , white      , 2)
      'R' -> (Nothing, red        , 2)
      'O' -> (Nothing, orange     , 2)
      'V' -> (Nothing, violet     , 3)
      'G' -> (Nothing, green      , 1)
      'B' -> (Nothing, azure      , 1)
      'Y' -> (Nothing, yellow     , 1)
      'X' -> (Nothing, haskellPurple1, 1) -- shades of Haskell functional purple 
      'I' -> (Nothing, haskellPurple2, 1)
      'Z' -> (Nothing, haskellPurple3, 1)
      _   -> (Nothing, magenta    , 1) -- For an unrecognized character

