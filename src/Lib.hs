module Lib where


import Graphics.Gloss (Color, Picture)
import Data.Map ( Map )
import qualified Data.Map as Map

{-
  Contains all Definitions for all Game data types
-}


data Ball   = Ball   {
    radius      :: Float,
    ballPosX    :: Float, -- coordinates of ball center
    ballPosY    :: Float,
    ballVelX    :: Float, -- x and y velocity
    ballVelY    :: Float,
    ballColour  :: Color,
    ballBonuses :: [Bonus] -- List of effects which should be applied to the ball each game update
} deriving (Eq, Show)

data Paddle = Paddle {
    paddlePosX      :: Float, -- the center of the paddle, is updated with cursor's x coordinate
    paddlePosY      :: Float, -- Remains unchanged
    paddleLength    :: Float,
    paddleThickness :: Float
}

data Brick = Brick {
    brickColor          :: Color,
    -- Each time a ball is collided with, decrease its durability
    -- by 1. Will turn to Empty once fully broken
    brickDurability     :: Int,
    -- Store the initial durability to calculate how much durability is lost
    -- This allows bricks to be rendered darker for each durability point lost
    brickMaxDurability  :: Int,
    -- Certain bricks contain bonuses which become active when
    -- the brick is broken
    brickBonus          :: Maybe Bonus
  }
    | Empty -- the state for broken bricks 
    deriving (Eq, Show)

data World = World  {
    paddle                     :: Paddle,
    balls                      :: [Ball],  -- All balls on screen
    currentLevels              :: [Level], -- Levels which aren't yet fully cleared
    allLevels                  :: [Level], -- Store original state of all levels to reset currentLevels after a game ends
    state                      :: State,   -- Whether game is running or if waiting for input to start a level/respawn a ball
    score                      :: Int,     -- Score of current attempt
    lives                      :: Int,     -- When a ball falls off the screen, decrease by one
    timer                      :: Float,   -- Is reset at the start of each level
    storedHighscore            :: Int,     -- Store current highscore for rendering in game
    logoImg                    :: Picture,
    t                          :: Float    -- For creating text animations, track a "ticks" parameter
}

data Bonus =
    LargePaddle     | -- Increases paddle length by a constant factor each time it is consumed 
    MultiBall       | -- Spawns 3, faster than normal, balls at the paddle position
    IndestructiBall   -- Increases a ball's size and makes it immune to brick collisions
    deriving (Show, Eq)

data State = Title              | -- Display game logo
             Play               | -- update bricks, apply ball physics
             GameEnd EndReason  | -- show score, update highscore
             StartLevel  -- wait for user to press space and respawn ball/start next level 
    deriving (Show, Eq)

-- A game can end for only 3 reasons:
data EndReason = Win | NoLives | NoTime
    deriving (Show, Eq)

data Level = Level {
    levelRows :: Int,
    levelCols :: Int,
    levelBrickDims :: (Float, Float), -- Can vary per level, and is used for drawing bricks/accessing the Bricks Map with the correct key pair
    -- Maps brick (row, column) to the brick at that row and column
    -- Ball coordinates can be easily converted to brick row-column pairs based on brick width and length
    bricks :: Map (Int, Int) Brick
}