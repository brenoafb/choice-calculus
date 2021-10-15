{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Var where

import Data.List (elemIndex, intercalate)
import Data.Generics
import Data.Generics.Uniplate.Data

type Dim = String
type Tag = String
type Tagged a = (Tag, V a)

infixl 2 <:

(<:) :: Tag -> V a -> Tagged a
t <: v = (t, v)

data V a = Obj a
         | Dim Dim [Tag] (V a)
         | Chc Dim [V a]
         deriving (Data, Typeable)

instance Eq a => Eq (V a) where
  (Obj x) == (Obj y) = x == y
  (Dim d ts v) == (Dim d' ts' v')
    | d == d' && ts == ts' = v == v'
  (Chc d vs) == (Chc d' vs')
    | d == d' = vs == vs'
  _ == _ = False

instance Show a => Show (V a) where
  show (Obj x) = show x
  show (Dim d ts v) =
    "dim " <> d <> "<" <> intercalate ", " ts <> "> in " <> show v
  show (Chc d vs) =
    d <> "<" <> intercalate ", " (map show vs) <> ">"

instance Functor V where
  fmap f (Obj x) = Obj $ f x
  fmap f (Dim d ts c) = Dim d ts $ fmap f c
  fmap f (Chc d cs) = Chc d $ fmap (fmap f) cs

bindV :: V a -> (a -> V b) -> V b
(Obj xa)     `bindV` f = f xa
(Dim d ts v) `bindV` f = Dim d ts (v >>= f)
(Chc d cs)   `bindV` f = Chc d (map (>>= f) cs)

decide :: Data a => [(Dim, Tag)] -> V a -> V a
decide ds v = foldr (uncurry eliminate) v ds

eliminate :: Data a => Dim -> Tag -> V a -> V a
eliminate d t = transform f
  where f x@(Dim d' ts v)
          | d /= d' = x
          | otherwise =
            case elemIndex t ts of
              Nothing -> x
              Just i  -> go i v
        f x = x
        go i x@(Chc d' vs)
          | d /= d' = x
          | otherwise = vs !! i
        go i x = x

instance Applicative V where
  pure = Obj
  mf <*> mx = mf `bindV` (\f -> mx `bindV` (pure . f))

instance Monad V where
  (>>=) = bindV

liftV :: (a -> V b) -> V a -> V b
liftV = (=<<)

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
