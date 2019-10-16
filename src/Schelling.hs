-- 20027038 scyrs1 RICHARD SUGIANTO SO
module Schelling
  ( Coord
  , AgentType (..)
  , Cell

  , step
  ) where

import System.Random

-- import Data.List module to add in delete
import Data.List

-- type-definition of a coordinate in our world
type Coord = (Int, Int)

-- data definition for the agent types
data AgentType 
  = Red       -- ^ Red agent
  | Green     -- ^ Green agent
  | Blue      -- ^ Blue agent
  deriving Eq -- Needed to compare for equality, otherwise would need to implement by ourself

-- Type-definition of a Cell: it is just a coordinate and an optional AgentType.
-- Note: the agent type is optional and is Nothing in case the cell is empty.
type Cell = (Coord, Maybe AgentType)

-- Computes one step of the Schelling Segregation model. The algorithm works as:
--  1. mark all unhappy agents
--  2. move all unhappy agents to any random Emtpy cell
--  3. all happy agents stay at the same position
step :: [Cell]           -- ^ All cells of the world
     -> StdGen           -- ^ The random-number generator
     -> Double           -- ^ The ratio of equal neighbours an agent requires to be happy
     -> ([Cell], StdGen) -- ^ Result is the new list of cells and the updated random-number generator
step cs g _ratio = (cs', g')
  where
    (cs', g') = moveCell cs _ratio g

-- isHappy returns True if an agent is happy
-- false otherwise
isHappy :: [Cell] -> Double -> Cell -> Bool
isHappy cs _ratio ((x,y),t) = 
  -- An Agent is happy when the number of the same AgentType `div` the number of its neighbour is > _ratio
  if (xx / yy) >= (_ratio) then True
  else False
    where
      -- realCoord returns a list of Cell which exist in the world
      realCoord :: [Coord] -> [Cell]
      realCoord coord = filter (\(c,_) -> any (==c) coord) cs

      -- moore -> 8 of the surrounding coordinate
      moore = [(x-1, y-1), (x-1, y), (x, y-1), (x+1, y+1), (x+1, y), (x, y+1), (x+1, y-1), (x-1, y+1)]
      
      -- xs is a list of:
      -- all the cells that are in the neighbourhood and of the same AgentType
      xs = [a | a <- realCoord moore, isSameElem ((x,y),t) a]
      xx = fromIntegral (length xs)

      -- ys is a list of:
      -- all the Cells that are in the neighbourhood
      ys = [a | a <- realCoord moore]
      yy = fromIntegral (length ys)

      -- isSameElem returns True if 2 cells have the same AgentType
      -- return False if the 2 cells doesn't have the same AgentType
      isSameElem :: Cell -> Cell -> Bool
      isSameElem (_, a') (_, b') =
        if a' == b' then True
        else False

{- Logic of moveCell:
  csHappy = kept for updating the world
  take the head $ csUnhappy, take the elem and move it into movedcsUnhappy, recurse until length $ csUnhappy == 0
    csEmpty will always have more elem than csUnhappy
    the empty cell and unhappy cell that is chosen is removed
    end of recursion: there should be some elem in csEmpty -> which is the leftover elem (not choosen by g)
  update the new position of csEmpty:
    the old coordinate of csUnhappy is changed with unhappy2Empty -> becomes the new csEmpty
    these coordinates are then ++ with csEmpty -> to have all cells of csEmpty
  movedcsUnhappy contains the moved csUnhappy elem
-}

-- moveCell takes a world, _ratio, g and returns the new world
moveCell :: [Cell] -> Double -> StdGen -> ([Cell], StdGen)
moveCell cs _ratio g = (cs', g')
  where
    csEmpty = filter isEmpty cs -- Empty cells
    csNonEmpty = filter (not . isEmpty) cs  -- NonEmpty cells
    csHappy = filter (isHappy cs _ratio) csNonEmpty -- Happy cells
    csUnhappy = filter (not . isHappy cs _ratio) csNonEmpty -- Unhappy cells

    (movedcsUnhappy, leftOverEmpty, g') = swapCell g csUnhappy csEmpty []
    cs' = csHappy ++ movedcsUnhappy ++ leftOverEmpty 

    -- unhappy2Empty changes an unhappy cell to an empty cell
    unhappy2Empty :: Cell -> Cell
    unhappy2Empty (c,_) = (c, Nothing)

    -- isEmpty returns true when there is no agent
    -- and returns false when there is an agent
    isEmpty :: Cell -> Bool
    isEmpty (_,t) = 
      if t == Nothing then True
      else False

    -- swapCell takes a list of unhappy cells, empty cells
    -- returns movedcsUnhappy and the leftover empty cells
    swapCell :: StdGen -> [Cell] -> [Cell] -> [Cell] -> ([Cell], [Cell], StdGen)
    swapCell gSwap uh eh newUnhappy =
      if uh /= [] then
        -- swap (leftover unhappy) (leftover empty) (new position of the unhappy agnets)
        swapCell gSwap' (tail uh) leftEmpty tempUnhappy
      else (newUnhappy, eh, gSwap) -- compute the new unhappy elem and leftover empty elem
        where
          -- compute the random number chosen to choose which empty cell
          -- range is from 0 to length of empty cells -1 (due to indexing)
          (r, gSwap') = randomR(0, (length eh) -1) gSwap

          --tempUnhappy takes a list of the moved unhappy agents and add the new moved unhappy agents
          tempUnhappy = newUnhappy ++ [ swap (head uh) (eh !! r)]

          -- leftEmpty contains a list of the leftover empty cells
          -- update the empty cells with prev pos for unhappy
          leftEmpty = ( delete (eh !! r) eh) ++ [unhappy2Empty (head uh)]

          -- swap takes in an unhappy agent, and an empty cell
          -- and move that unhappy agent to the empty cell 
          -- returns the moved unhappy agent
          swap :: Cell -> Cell -> Cell
          swap (_,t) (c',_) = (c', t)