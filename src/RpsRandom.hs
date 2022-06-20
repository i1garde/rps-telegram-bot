module RpsRandom (randomItem) where

import RpsGame (RPS (..), RoundOutcome (..))
import System.Random
    ( UniformRange, Uniform, uniform, getStdRandom )
import System.Random.Stateful (uniformRM, Uniform (uniformM))

instance UniformRange RPS where
  uniformRM (lo,hi) rng = do
    res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
    pure $ toEnum res

instance Uniform RPS where
  uniformM = uniformRM (minBound, maxBound)

class RandomRps repr where
  getRandItem :: (Uniform a) => repr a

newtype R a = R { unR :: IO a }

instance RandomRps R where
  getRandItem = R $ getStdRandom uniform

evalRand :: R a -> IO a
evalRand = unR

randomItem :: IO RPS
randomItem = evalRand getRandItem