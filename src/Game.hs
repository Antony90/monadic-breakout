{-# LANGUAGE RecordWildCards #-}
module Game where

import Lib (World(..), Ball (..), Brick (..), Paddle(..), State(..), Bonus (LargePaddle, MultiBall, IndestructiBall), Level (..), EndReason (NoTime, NoLives, Win))
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Initialize (initialBall, bottom, levelTimer, height, width, right, left, top, haskellPurple1, haskellPurple2, haskellPurple3)
import Graphics.Gloss.Data.Color (green, dark, orange, red, yellow, azure)

{-
  Contains all functions which update the World 
    - Update Ball/Paddle velocities/effects
    - Update State following game events
    - Update Brick Map with collisions
  
  All functions are composed to form `tickWorld`
  which is the only function called in the Main module

  Bricks are organized in a `Map (Int, Int) Brick`
  where keys are (i, j) for a brick at the i th row and j th column
  on screen. Ball coordinates can be easily converted 
  to brick row-column pairs using brick width, length and
  screen size.
  
  This is an improvement over a single list of [Brick]
  as we now require at most 4 Bricks to be accessed
  each frame - one for each possible collision point on
  the ball's circumference along each axis.
-}





{-
 - Update the World by checking for collisions with Bricks in the Map 
 - by converting on screen ball positions to indexes in the Bricks Map/
 -
 - We can convert the coordinates at the top, bottom, left and right most points on
 - each Ball's circumference, to an index pair (i, j) which corresponds to the
 - brick at the i th row and j th column on screen.
 -
 - For each Brick we access with these coordinates, apply a collision
 - lowering its durability and possibly breaking it when it has none left.
 - Also, update the score and apply any Bonus effects from the collided bricks
 -
 - Collided bricks are also separated by X and Y axis so Ball velocity 
 - can be reflected on each axis.
 -
 - There are three end game states: Clearing the bricks of all levels, running out of time
 - on any level and loosing all lives.
 -
-}
tickWorld :: World -> Float -> World
tickWorld world@World{..} time =
  case currentLevels of
    -- When we run out of levels, the player has won
    []                          -> world { state = GameEnd Win }
    (currentLevel@Level{..}:lvls) ->
      world {
          balls            = if noBalls || allBricksCleared 
                              then [initialBall] -- reset the ball when the Level ends or to respawn a ball 
                              -- otherwise, update ball velocities based on collided bricks
                              -- and spawn more balls if a MultiBall brick is broken 
                              else updatedBalls ++ if MultiBall `elem` newBonuses
                                                      -- MultiBalls spawned are centered to the paddle
                                                      then multiBalls paddle 
                                                      else [],

          currentLevels    = if allBricksCleared
                              then lvls                 -- remove the current level if it has no Bricks
                              else updatedLevel:lvls,   -- or replace head with the updated level

          -- apply LargePaddle bonus when its bonus brick is broken
          paddle           = if LargePaddle `elem` newBonuses 
                              then paddle { paddleLength = 1.5 * paddleLength paddle }
                              else paddle,

          -- add the score of each collided brick
          score            = score + (sum . map brickScore) allCollidedBricks,
          lives            = updatedLives, -- when no balls remain 1 life is deducted 
          state            = updatedState,

          -- if the level is cleared, reset the level timer
          timer            = if allBricksCleared then levelTimer else timer - time
      }

      where
          -- Controls transitions between levels and between games
          updatedState | allBricksCleared &&                    -- When all bricks have been cleared and there
                          (not . null) lvls   = StartLevel      -- exist more levels, wait for user to start the next.
                       | updatedLives == 0    = GameEnd NoLives -- When game is over, update high score 
                       | timer < 0            = GameEnd NoTime       
                       | noBalls              = StartLevel      -- No balls, wait for user to restart level
                       | otherwise            = Play            -- Otherwise, continue updating the current level                 

          updatedBalls         = filter
                                  -- remove balls below the bottom edge
                                  (\Ball {..} -> ballPosY >= bottom) $
                                  -- update each ball based on its collisions in each axis
                                  zipWith
                                  (`updateBall` paddle)
                                  -- The i th collision pair in `ballBrickCollisions` 
                                  -- corresponds to the collisions made by i th ball
                                  -- in `balls`. Can be considered in parallel
                                  balls
                                  -- Represents each ball's possible collision with a brick in each axis as a pair of `Maybe Brick`
                                  ballBrickCollisions
          -- zipWith is used here so `ballBrickCollisions` can be separated and passed to
          -- `allCollidedBricks` which can be used by other world updating functions
          -- that rely on brick collisions e.g. finding bonuses, updating score.

          -- Updates bricks by accessing the Bricks Map at the ball's collision points
          updatedLevel         = updateLevel currentLevel balls
          -- Each time no balls are on screen, deduct a life
          updatedLives         = lives - (if noBalls then 1 else 0)

          allBricksCleared     = hasNoBricks updatedLevel
          noBalls              = null updatedBalls

          -- Maps each ball to a pair of Maybe Brick, representing a possible
          -- collision with a brick in the X and Y axis respectively
          -- Separating collisions by axis allows ball velocities to be
          -- updated component-wise  
          ballBrickCollisions :: [(Maybe Brick, Maybe Brick)]
          ballBrickCollisions = collidedBricks balls bricks levelBrickDims

          -- Reduce collision pairs to a single list of collided bricks.
          -- Note: Collided bricks are not necessarily broken bricks
          allCollidedBricks :: [Brick]
          allCollidedBricks = catMaybes $ concatMap (\(x, y) -> [x, y]) ballBrickCollisions

          brokenSpecialBricks :: [Brick]
          brokenSpecialBricks = filter (\Brick{..} -> isJust brickBonus && brickDurability == 1) allCollidedBricks

          -- Extract all bonuses from broken special bricks.
          -- MultiBall and LargePaddle can be applied in this function
          -- Ball level bonuses (IndestructiBall) are extracted and applied in `updateBall`
          newBonuses :: [Bonus]
          newBonuses = mapMaybe brickBonus brokenSpecialBricks

-- | Maps each ball to a pair of Maybe Bricks, representin
-- a possible collision with a brick in the X and Y axis respectively.
-- This allows x and y velocities to be updated when a Brick is collided with
-- in its direction.
collidedBricks :: [Ball] -> Map (Int, Int) Brick -> (Float, Float) -> [(Maybe Brick, Maybe Brick)]
collidedBricks balls bricks levelBrickDims =
  map
    (\ball ->
      getAxisCollisionPair ball bricks levelBrickDims
    ) balls

-- | Gets the pair of bricks a ball has collided into for the X and Y axis
-- by accessing the Bricks Map at the: top, bottom, left and right most points
-- on the circle's circumference. To do this, screen coordinates must be converted
-- to brick (column, row) coordinates.
--
-- This provides 4 Bricks the ball may be in contact with. It is only possible to
-- be in contact with 1 brick in each axis (under normal circumstances) so,
-- only consider the first non `Empty` list element.
getAxisCollisionPair :: Ball -> Map (Int, Int) Brick -> (Float, Float) -> (Maybe Brick, Maybe Brick)
getAxisCollisionPair ball bricks brickDims =
   -- convert the x and y collision coordinates into brick (column, row)
   -- Then lookup the key in the Bricks Map, taking the first result
    ( getCollidedBrick $ xBrickCollisionIndexes ball brickDims, 
      getCollidedBrick $ yBrickCollisionIndexes ball brickDims )
    where
      getCollidedBrick :: [(Int, Int)] -> Maybe Brick
      getCollidedBrick = 
          -- It is impossible to collide with more than one brick along
          -- each axis so, taking the first element is valid.
          -- If the list is empty, no collision occured on this axis, return Nothing
          -- If any collision occurs, we get (Just Brick)
          safeHead
          -- ignore Empty bricks
          . filter (Empty /=)
          -- safely lookup the brick in the map at each point
          . mapMaybe (`Map.lookup` bricks)

      safeHead :: [a] -> Maybe a
      safeHead xs = case xs of
        [] -> Nothing
        _  -> Just $ head xs

-- | Converts screen coordinates to corresponding
-- brick (row, column) pair to index the Bricks Map at.
toBrickIndicies :: (Float, Float) -> (Float, Float) -> (Int, Int)
toBrickIndicies (posX, posY) (brickWidth, brickLength) =
  let i = floor $ ((fromIntegral width  / 2) + posX) / brickWidth
      j = floor $ ((fromIntegral height / 2) - posY) / brickLength
  in (i, j)

-- | Take the left and right most points on the ball and
-- convert it to a brick (column, row) coordinate.   
-- Accessing the Brick Map with these coordinates as keys
-- returns bricks the ball is currently in contact with in the x axis
xBrickCollisionIndexes :: Ball -> (Float, Float) -> [(Int, Int)]
xBrickCollisionIndexes Ball{..} brickDims = map (`toBrickIndicies` brickDims)
  [ (ballPosX + radius + ballVelX, ballPosY + ballVelY),
    (ballPosX - radius + ballVelX, ballPosY + ballVelY) ]

-- | Same as for xBrickCollisionIndexes but we consider points above
-- and below the ball's center for y axis brick collision.
yBrickCollisionIndexes :: Ball -> (Float, Float) -> [(Int, Int)]
yBrickCollisionIndexes Ball{..} brickDims = map (`toBrickIndicies` brickDims)
  [ (ballPosX + ballVelX, ballPosY + radius + ballVelY),
    (ballPosX + ballVelX, ballPosY - radius + ballVelY) ]




-- When a brick has a durability of 1, 
-- its collision will break the brick.
-- We should only get a score for breaking bricks
brickScore :: Brick -> Int
brickScore Brick{ brickDurability = 1, brickColor = brickColor }
  | brickColor == red    = 300           -- pattern matching does not work on Graphics.Gloss.Data.Color
  | brickColor == orange = 200
  | brickColor == yellow = 150
  | brickColor == azure  = 100
  | brickColor == green  = 50
  | brickColor == haskellPurple1
    || brickColor == haskellPurple2
    || brickColor == haskellPurple3  = 50
  | otherwise            = 10
-- Empty or not to-be-broken Brick
-- Empty pattern is never reached since
-- collided bricks are always non-empty
brickScore _         = 0 


-- | A level is complete when all of its bricks are
-- Empty or there are no `Brick`s
hasNoBricks :: Level -> Bool
hasNoBricks Level{..} = (not . any (Empty /=)) (Map.elems bricks)


-- | After consuming the MultiBall bonus, 3 balls
-- are spawned at the centre of the paddle with opposing
-- velocities.
multiBalls :: Paddle -> [Ball]
multiBalls Paddle{..} =
  map (\velX ->
        Ball {
          radius = 9.0,
          ballPosX = paddlePosX,
          ballPosY = -260,
          ballVelX = velX,
          ballVelY = 7,
          ballColour = dark green,
          ballBonuses = []
        }
      ) [-3, 0, 3]



-- | The bricks in a level need to be removed/have durability updated
-- each time a ball collides with it. We update all bricks a ball collides
-- with, by accessing the Map with the top, bottom, left and right most points
-- on the ball. 
--
-- These coordinates need to be converted to 
-- brick (column, row) pairs before doing so.
--
-- Since Data.Map is immutable, a fold is used to accumulate the
-- updated the map and update it for each ball collision point.
--
-- Another fold is used to update the Map for every ball
updateLevel :: Level -> [Ball] -> Level
updateLevel l@Level {..} balls = l { bricks = updatedBricks }
  where
    -- Accessing the Map at the ball's collision points
    -- converted to brick row, column indexes.
    -- Update the bricks stored under these keys.
    updateAllBricks :: Ball -> Map (Int, Int) Brick -> Map (Int, Int) Brick
    updateAllBricks ball bricks =
      -- use a fold to update the Map at all indexes 
      foldr
        (Map.update updateBrick)
        bricks
        (allCollisionIndexes ball)

    -- for all balls, update collided bricks in the Bricks Map
    updatedBricks :: Map (Int, Int) Brick
    updatedBricks = foldr updateAllBricks bricks balls

    -- Update a brick following its collision.
    -- This can decrease durability or break the brick.
    updateBrick :: Brick -> Maybe Brick
    updateBrick b@Brick{..} =
      case brickDurability of
          1 -> Just Empty -- break the brick
          x -> Just b { brickDurability = x - 1 }
    updateBrick Empty = Nothing -- when the brick is Empty, it is unchanged

    -- Consider points along both axis 
    allCollisionIndexes ball = yBrickCollisionIndexes ball levelBrickDims ++
                               xBrickCollisionIndexes ball levelBrickDims

-- | A ball's position is incremented by its velocity each
-- game frame. Ball velocity can be reflected depending on
-- which axis a ball makes a collision on. 
-- When a ball collides with the paddle, its X velocity is set
-- such that it moves to the right when it hits the right side,
-- and the opposite for the left side.
--
-- Collisions are separated into the X and Y axis since we only need
-- to reflect the ball velocity in each axis when a Brick is present
-- in the tuple position for that axis.
--
-- The bonus of in each collided brick and can then be extracted
-- and applied.
--
-- I have only implemented one bonus which affects ball physics however,
-- I designed this with upgadeability in mind - it is straightforward
-- to add more bonus effects.
--
-- Bonus: 
--    IndestrictiBall - makes ball larger and ignores bricks collision
updateBall :: Ball -> Paddle -> (Maybe Brick, Maybe Brick) -> Ball
updateBall ball@Ball {..} paddle (xCollidedBrick, yCollidedBrick) =
  ball {
      ballPosX     = newX,
      ballPosY     = newY,
      ballVelX     = newVelX,
      ballVelY     = newVelY,
      ballBonuses  = updatedBonuses,
      radius       = if indestructiBallBonus then 20 else radius
    }
  where
      updatedBonuses = newBonuses ++ ballBonuses

      -- extract bonuses from both collisions
      newBonuses = mapMaybe exctractBonus [xCollidedBrick, yCollidedBrick]
      -- only when the brick is about to break
      exctractBonus (Just Brick { brickDurability = 1, brickBonus = b }) = b
      exctractBonus _ = Nothing

      -- Bonus effects:
      -- * IndestrictiBall: no velocity reflection from brick collisions
      indestructiBallBonus = IndestructiBall `elem` updatedBonuses

      -- When collided with borders or bricks, reflect the ball's
      -- velocity along that axis
      xMult
        |  ballPosX - radius + ballVelX <= left 
        || ballPosX + radius + ballVelX >= right
        || isJust xCollidedBrick && not indestructiBallBonus  = -1 -- reflect x velocity
        |  otherwise                                          = 1

      yMult
        |  ballPosY + radius + ballVelY >= top
        || ball `isOnPaddle` paddle
        || isJust yCollidedBrick && not indestructiBallBonus  = -1 -- reflect y velocity
        |  otherwise                                          = 1

      newVelY = yMult * ballVelY

      -- As per the original game:
      -- When ball is on paddle, set x velocity negative if on left side, positive otherwise
      -- Set x velocity to the normalized distance from centre of paddle
      -- and multiply by factor of 13 for larger effects in game
      newVelX = if ball `isOnPaddle` paddle
                then
                  ball `normalizedDistanceFromCentre` paddle * 13
                else -- otherwise, it collides with the ceiling or brick, so reflect as usual
                  xMult * ballVelX

      (newX, newY) = (ballPosX + newVelX, ballPosY + newVelY)


isOnPaddle :: Ball -> Paddle -> Bool
Ball {..} `isOnPaddle` Paddle {..} =
    ballPosX + ballVelX >= paddlePosX - (paddleLength / 2)
        && ballPosX + ballVelX <= paddlePosX + (paddleLength / 2)
        && ballPosY - radius + ballVelY <= paddlePosY
        && ballPosY - radius + ballVelY >= paddlePosY - paddleThickness

-- | A number in [-1, 1] representing the ball's displacement from centre of paddle
normalizedDistanceFromCentre :: Ball -> Paddle -> Float
Ball {..} `normalizedDistanceFromCentre` Paddle {..} = (ballPosX - paddlePosX) / paddleLength

