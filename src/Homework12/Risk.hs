{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework12.Risk where

import Data.List

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle b = kill b (deaths (matchDie (fighters b)))

fighters :: Battlefield -> Battlefield
fighters b = Battlefield { attackers = min 3 (attackers b)
                         , defenders = min 2 (defenders b) }

rollDiceAndSort :: Int -> Rand StdGen [DieValue]
rollDiceAndSort n =
  replicateM n die
    >>= (return . sortBy (flip compare))


-- battle b = go $ fighters b
--   where
matchDie :: Battlefield -> Rand StdGen [(DieValue, DieValue)]
matchDie (Battlefield {attackers = atk, defenders = def}) =
  rollDiceAndSort atk
    >>= (\as -> rollDiceAndSort def
          >>= (\ds -> return $ as `zip` ds))

deaths :: Rand StdGen [(DieValue, DieValue)] -> Rand StdGen (Int, Int) -- Rand StdGen (Battlefield)
deaths mas = mas
  >>= (\xs -> return $ fmap (\(a, d) -> if a > d then (0, 1) else (1, 0)) xs)
  >>= (\ys -> return $ (foldr ((+) . fst) 0 ys, foldr ((+) . snd) 0 ys))

kill :: Battlefield -> Rand StdGen (Int, Int) -> Rand StdGen Battlefield
kill (Battlefield { attackers = atks, defenders = defs }) ds =
  ds >>= (\(ads, dds) -> return $ Battlefield { attackers = atks - ads, defenders = defs - dds})
