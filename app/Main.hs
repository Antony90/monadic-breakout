{-# LANGUAGE RecordWildCards #-}
module Main where

{-
  Contains the main game loop
  `draw`         - Render the world in Picture form
  `updateWorld`  - Passes the World to a pure function.
                   Will step the game one tick forward and
                   writes the new highscore to a text file 
                   following each game.
  `inputHandler` - Handles transitions between levels, start/end of game
                   and respawning balls.
-}


import Graphics.Gloss.Interface.IO.Game
    ( Display (InWindow),
      Event (EventKey, EventMotion),
      Key (Char, SpecialKey),
      KeyState (Down, Up),
      Picture (Blank),
      SpecialKey (KeyEnter, KeyLeft, KeyRight, KeySpace, KeyUp),
      circleSolid,
      color, dark, black, red, white, green, makeColorI,
      pictures,
      playIO,
      rectangleSolid,
      scale,
      text, translate, magenta, mixColors )

import Lib
    ( Ball(..),
      Bonus(MultiBall, LargePaddle, IndestructiBall),
      Brick(..),
      Level(..),
      Paddle(..),
      State(StartLevel, Play, GameEnd, Title),
      World(..),
      EndReason(Win, NoLives, NoTime) )

import Initialize
    ( height,
      initialBall,
      makeWorld,
      left,
      resetWorld,
      right,
      top,
      width, haskellPurple1, haskellPurple2, haskellPurple3 )

import Game ( tickWorld )

import System.IO ( hClose, openFile, hPutStr, IOMode(WriteMode), hPrint, readFile )
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map (lookup)
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Control.Monad (when)

-- | Takes a list of Picture and composes them into one
--   Picture to render above the background using `pictures`
draw :: World -> IO Picture
draw word@World {..} = return $ pictures $
    case state of
      -- Render logo and saved highscore
      Title -> [
          translate (-50) 0 logoImg,
          pulsingText t $ translate (-250) (-360) $ scale 0.2 0.2 $ text "Press ENTER to start the game",
          color magenta $ translate (-45) 250 $ scale 0.8 0.8 $ text "Monadic",
          color haskellPurple3 $ translate 100 160 $ scale 0.45 0.45 $ text "Breakout",
          color white $ translate 206 (-80) $ scale 0.12 0.12 $ text $ "Highscore: " ++ show storedHighscore]

      -- Game is live, render current level's Brick Map, Balls, Paddle, timer, lives, score
      Play -> currentLevelPictures

      -- Game ends with 3 reasons: No lives, No time or Win
      -- Render the reason, score and the saved highscore
      GameEnd reason ->
          let
            noLives = lives == 0
            noTime  = timer < 0
            lost    = noLives || noTime
            -- Render some text a different colour in the end screen
            accent = if lost then red else green
          in
          [
            translate (-300) 200 $ scale 0.7 0.7 $ color accent $ text (if lost then "GAME OVER!" else " YOU WIN!"),
            translate (-350) (-200) $ color accent $ scale 0.3 0.3 $
              text $ (case reason of
                NoLives   -> "You ran out of Lives"
                NoTime    -> "You ran out of time"
                Win       -> "You cleared all levels") ++ " with Score: " ++ show score ++ "!",

            translate (-200) (-250) $ pulsingText t $ scale 0.20 0.20 $ text "Press ENTER to play again",
            translate (-150) (-285) $ color white   $ scale 0.15 0.15 $ text $ "Current Highscore: " ++ show storedHighscore,
            translate 0 40 $ scale 0.4 0.4 logoImg
          ]

      -- Waiting for user to start level with SPACE key
      StartLevel  ->
         currentLevelPictures ++ [pulsingText t $ translate (-140) (-320) $ scale 0.2 0.2 $ text "Press SPACE to start"]

  where
    pulsingText t = let r = (sin t + 1)/2 in 
      color $ mixColors r (1-r) white black

    currentLevelPictures :: [Picture]
    currentLevelPictures = case currentLevels of
      []                -> [Blank]
      (currentLevel:ls) -> drawLevel currentLevel

    -- Combines all text, balls and bricks
    drawLevel :: Level -> [Picture]
    drawLevel currentLevel =
      paddlePicture paddle :
       livesText           :
       scoreText           :
       highscoreText       :
       timerText           :
       brickPictures currentLevel ++
       map ballToPicture balls

    livesText     = translate (left+10)   370 $ scale 0.2 0.2 $ color white $ text $ "Lives: " ++ show lives
    scoreText     = translate (right-200) 370 $ scale 0.2 0.2 $ color white $ text $ "Score: " ++ show score
    highscoreText = translate (left+10) 330 $ scale 0.2 0.2 $ color white $ text $ "Highscore: " ++ show storedHighscore
    timerText     = translate (-25)  (top-30) $ scale 0.2 0.2 $ color white $ text $ show $ floor timer

    -- The Paddle is a rectangle centered at it's X position
    -- Is always aligned with mouse's X coordinate
    paddlePicture :: Paddle -> Picture
    paddlePicture Paddle {..} =
      translate paddlePosX (paddlePosY - paddleThickness / 2) $
        (color . dark . dark) red $
          rectangleSolid paddleLength paddleThickness

    -- If the ball has IndestrictiBall effect, 
    -- render as dark red
    ballToPicture :: Ball -> Picture
    ballToPicture Ball {..} =
      translate ballPosX ballPosY $
        color circleColor $
          circleSolid radius
      where circleColor = if IndestructiBall `elem` ballBonuses
                          then dark red
                          else ballColour

    -- Loop through every index of the Bricks Map grid
    -- and render the brick based on the current Map index
    brickPictures :: Level -> [Picture]
    brickPictures Level{..} = [
      brickToPicture (x, y) levelBrickDims $ Map.lookup (x, y) bricks
        | y <- [0 .. levelRows - 1],
          x <- [0 .. levelCols - 1] ]

    -- | Creates an image of each brick given its indexes in the Brick Map
    -- Bricks are rectangles outlined with a darker shade of their fill colour.
    -- For each point of durability lost, a brick appears darker.
    -- Bricks loose durability following collision with a Ball
    brickToPicture :: (Int, Int) -> (Float, Float) -> Maybe Brick -> Picture
    brickToPicture _ _ (Just Empty) = Blank -- Empty brick at position
    brickToPicture _ _ Nothing      = Blank -- Indicies are out of range, but should not occur
    brickToPicture (x, y) (brickWidth, brickLength) (Just brick@Brick {..}) =
        uncurry translate (fromBrickIndexes (x, y) brickWidth brickLength)
        $ pictures
          -- Draw the Brick rectangle 3 times to create a black spacing between bricks
          -- and an outline. Also make the brick's inner colour
          -- darker for each missing point of durability
          [ color black                         $ rectangleSolid brickWidth brickLength, -- black rectangle for empty space between bricks
            -- outline Brick with a darker shade
            (color . dark . dark) brickColor    $ rectangleSolid (brickWidth - 4) (brickLength - 4),
            -- Fill inner shape, accounting for lost durability
            color (durabilityColour brickColor) $ rectangleSolid (brickWidth - 15) (brickLength - 15),
            -- Add the bonus icon if it has one
            drawBonusIcon brick ]
      where
        -- for each durability missing, apply `dark` 3 times
        durabilityColour = foldr (.) id $ replicate (3*(brickMaxDurability - brickDurability)) dark

        -- Given a brick, if it has a bonus, return the corresponding bonus icon
        -- otherwise, Blank
        drawBonusIcon Empty = Blank
        drawBonusIcon Brick{..} = case brickBonus of
          Nothing              -> Blank
          Just LargePaddle     ->
            -- red rectangle at center of rectangle
            color red $ rectangleSolid (0.5*brickWidth) (0.14*brickLength)
          Just IndestructiBall ->
            -- red cirdle with asterisk at center
            pictures [color red $ circleSolid 12, color black $ translate (-11) (-15) $ scale 0.35 0.35 $ text "*"]
          Just MultiBall       ->
            -- 3 small green balls in the middle
            pictures $ map ((color . dark) green)
              [
                translate (-10) (-4) $ circleSolid 6.5,
                translate 0    12    $ circleSolid 6.5,
                translate 10    (-4) $ circleSolid 6.5
              ]

-- | Used by `draw` to convert a pair of Brick Map indexes (x, y)
-- (where x, y is the row and column of a brick to draw on screen) to
-- on screen coordinates.
fromBrickIndexes :: (Int, Int) -> Float -> Float -> (Float, Float)
fromBrickIndexes (x, y) brickWidth brickLength =
    (  fromIntegral x * brickWidth  +                  (brickWidth / 2) - (fromIntegral width  / 2),
     -(fromIntegral y * brickLength) - (brickLength / 2)                + (fromIntegral height / 2) )



-- When a game has ended, update highscore.txt and the highscore in World, once.
updateWorld :: Float -> World -> IO World
updateWorld time world@World {..} =
  case state of
    Title       -> return $ world { t = t + 0.07 }  -- While waiting for a user input,
    StartLevel  -> return $ world { t = t + 0.07 }  -- update ticks for text animation
      -- If the game is over, update the highscore
      -- only once.
    GameEnd _   ->
      -- Only when we beat our previous highscore,
      -- save it to highscore.txt and update
      -- the record in `World`
      if score > storedHighscore then 
        do
          updateHighscore score
          -- save the highscore in a record for convenience
          return $ world { storedHighscore = score }
      else
        return world { t = t + 0.07 } -- update animation ticks
    -- Main game loop, update ball physics, apply collisions
    Play        -> return $ tickWorld world time



-- Updates the previously saved highscore to the new one.
updateHighscore :: Int -> IO ()
updateHighscore score =
    do
      -- replace contents of the file 
      -- with the new highscore
      highscoreH <- openFile "assets/highscore.txt" WriteMode
      hPrint highscoreH score
      hClose highscoreH

-- When the game first launches, get the saved highscore
-- and save it to `World`
getHighscore :: IO Int
getHighscore =
  do
    highscoreStr <- readFile "assets/highscore.txt"

    -- if invalid contents/empty file, treat score as 
    -- having no previous highscore
    return $ fromMaybe 0 (readMaybe highscoreStr)
    



-- SPACE starts a level
inputHandler :: Event -> World -> IO World
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) w@World{..} = return $
  case state of
    StartLevel  -> w { state = Play }
    _           -> w


-- ENTER transitions from Title/GameEnd first level
inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) w@World{..} = return $
  case state of
    Title       -> w { state = StartLevel }
    GameEnd _   -> resetWorld w -- use `resetWorld`, avoids using `initialWord` which generates `Levels`
    _           -> w

-- When user is waiting to respawn a ball or start the next level,
-- update both the ball and paddle's positions with the cursor
inputHandler (EventMotion (x, y)) w@World { state = state, paddle = p@Paddle{..} } = return $
  case state of
    Title       -> ballPaddleFollowCursor x -- in these states, the game has not yet begun
    StartLevel  -> ballPaddleFollowCursor x -- so, lock the ball and paddle onto the cursor
    _           -> w { paddle = updatedPaddle } -- as always, the paddle follows the mouse
  where
    updatedPaddle = p { paddlePosX = x }
    ballPaddleFollowCursor x = w { paddle = updatedPaddle, balls = [initialBall {ballPosX = x}] }

-- A cheat for skipping the current level
inputHandler (EventKey (Char 'r') Down _ _) w@World{ currentLevels = (l:ls)} = return $ w { currentLevels = ls }
inputHandler _ world = return world


main :: IO ()
main =
  do
    logoImg <- loadBMP "assets/logo.bmp"
    level1Text <- readFile "assets/levels/level1.txt"
    level2Text <- readFile "assets/levels/level2.txt"
    level3Text <- readFile "assets/levels/level3.txt"

    -- load the old highscore, if empty contents use 0
    currentHighscore <- getHighscore

    playIO
      (InWindow "Monadic Breakout" (width, height) (300, 0))
      black
      60
      (makeWorld [level1Text, level2Text, level3Text] logoImg currentHighscore)
      draw
      inputHandler
      updateWorld
