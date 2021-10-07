module Var where

import Data.List (intersperse)

type Dim = String
type Tag = String
type Tagged a = (Tag, V a)

infixl 2 <:

(<:) :: Tag -> V a -> Tagged a
t <: v = (t, v)

data V a = Obj a
         | Dim Dim [Tag] (V a)
         | Chc Dim [V a]

instance Show a => Show (V a) where
  show (Obj x) = show x
  show (Dim d ts v) =
    "dim " <> d <> "<" <> (concat $ intersperse ", " ts) <> "> in " <> show v
  show (Chc d vs) =
    d <> "<" <> (concat . intersperse ", " $ map show vs) <> ">"

instance Functor V where
  fmap f (Obj x) = Obj $ f x
  fmap f (Dim d ts c) = Dim d ts $ fmap f c
  fmap f (Chc d cs) = Chc d $ fmap (fmap f) cs

bindV :: V a -> (a -> V b) -> V b
(Obj xa)     `bindV` f = f xa
(Dim d ts v) `bindV` f = Dim d ts $ (v >>= f)
(Chc d cs)   `bindV` f = Chc d (map (>>= f) cs)

instance Applicative V where
  pure = Obj
  mf <*> mx = mf `bindV` (\f -> mx `bindV` (\x -> pure $ f x))

instance Monad V where
  -- return :: a -> V a
  return = Obj

  -- (>>=) :: V a -> (a -> V b) -> V b
  (>>=) = bindV

liftV :: (a -> V b) -> V a -> V b
liftV = flip (>>=)

atomic :: Dim -> [Tag] -> [V a] -> V a
atomic d ts cs = Dim d ts $ Chc d cs

dimA :: V a -> V a
dimA = Dim "A" ["a1", "a2"]

chcA :: [V a] -> V a
chcA = Chc "A"

dimB :: V a -> V a
dimB = Dim "B" ["b1", "b2"]

chcB :: [V a] -> V a
chcB = Chc "B"

alt :: Dim -> [Tagged a] -> V a
alt d tvs = atomic d ts vs
  where (ts, vs) = unzip tvs
