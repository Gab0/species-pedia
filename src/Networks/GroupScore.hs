
module Networks.GroupScore where

import qualified Data.Map.Strict as Map
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import           Data.Strict.Tuple (Pair((:!:))) 

-- | This module deals with group scoring.
-- Source: https://hackage.haskell.org/package/nlp-scores-0.7.0
-- Copied required parts here since this library wouldn't compile.

-- | Mutual information: MI(X,Y) = H(X) - H(X|Y) = H(Y) - H(Y|X). Also
-- known as information gain.
mi :: (Ord a, Ord b) => Counts a b -> Double
mi (Counts cxy cx cy) =
  let n = Map.foldl' (+) 0 cxy
      cell (x :!: y) nxy = 
        let nx = cx Map.! x
            ny = cy Map.! y
        in  nxy / n * logBase 2 (nxy * n / nx / ny)
  in sum [ cell (x :!: y) nxy | (x :!: y, nxy) <- Map.toList cxy ]

-- | Creates count table 'Counts'
counts :: (Ord a, Ord b, T.Traversable t, F.Foldable s) => t a -> s b -> Counts a b
counts xs = F.foldl' f empty . zipWithTF (:!:) xs . F.toList
    where f cs@(Counts cxy cx cy) p@(x :!: y) = 
            cs { joint       = Map.insertWith (+) p 1 cxy
               , marginalFst = Map.insertWith (+) x 1 cx
               , marginalSnd = Map.insertWith (+) y 1 cy }

-- | A count.
type Count = Double

data Counts a b = 
  Counts 
  { joint :: !(Map.Map (Pair a b) Count) -- ^ Counts of both components
  , marginalFst :: !(Map.Map a Count) -- ^ Counts of the first component
  , marginalSnd :: !(Map.Map b Count) -- ^ Counts of the second component
  } 
  deriving (Show)

empty :: (Ord a, Ord b) => Counts a b
empty = Counts Map.empty Map.empty Map.empty

-- | @zipWithTF h t f@ zips the values from the traversable @t@ with
-- the values from the foldable @f@ using the function @h@.
zipWithTF :: (T.Traversable t, F.Foldable f) =>
             (a -> b -> c) -> t a -> f b -> t c
zipWithTF h t f = snd . T.mapAccumL map_one (F.toList f) $ t
  where 
      map_one (x:xs) y = (xs, h y x)
      --map_one []     y = ([], h y )