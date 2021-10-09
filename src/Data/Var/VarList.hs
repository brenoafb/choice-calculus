{-# LANGUAGE DeriveDataTypeable #-}
module Data.Var.VarList where

import Data.Var

import Data.Generics
import Control.Monad (join)

type VList a = V (List a)

data List a = Cons a (List a)
            | Empty
            | VList (VList a)
            deriving (Data, Typeable, Show)

list :: List a -> VList a
list = Obj

single :: a -> List a
single a = Cons a Empty

many :: [a] -> List a
many = foldr Cons Empty

vempty :: VList a
vempty = list Empty

vsingle :: a -> VList a
vsingle = list . single

vcons :: a -> VList a -> VList a
vcons x = list . Cons x . VList

vvcons :: V a -> VList a -> VList a
vvcons v vs = do
  x <- v
  xs <- vs
  pure $ Cons x xs

vlist :: [a] -> VList a
vlist = list . many

fold :: (a -> b -> b) -> b -> List a -> V b
fold _ b Empty = pure b
fold f b (Cons a l) = f a <$> fold f b l
fold f b (VList vl) = vl >>= fold f b

len :: List a -> V Int
len = fold (\_ s -> succ s) 0

vlen :: VList a -> V Int
vlen = liftV len

sumL :: Num a => List a -> V a
sumL = fold (+) 0

vsum :: Num a => VList a -> V a
vsum = join . fmap sumL


cat :: List a -> List a -> List a
cat Empty      r = r
cat (Cons a l) r = Cons a (l `cat` r)
cat (VList vl) r = VList ((`cat` r) <$> vl)

rev :: List a -> List a
rev Empty = Empty
rev (Cons x xs) = (rev xs) `cat` (single x)
rev (VList vl) = VList $ vl >>= (pure . rev)

vrev :: VList a -> VList a
vrev vs = vs >>= (pure . rev)

vcat :: VList a -> VList a -> VList a
-- vcat l r = list $ cat (VList l) (VList r)
vcat l r = cat <$> l <*> r

opt :: Dim -> a -> VList a
opt d x = atomic d ["yes", "no"] [vsingle x, vempty]

nth :: Int -> List a -> V a
nth _ Empty       = undefined
nth 1 (Cons x _)  = pure x
nth n (Cons _ xs) = nth (n-1) xs
nth n (VList vl)  = vl >>= nth n

vnth :: Int -> VList a -> V a
vnth n = liftV (nth n)

filterL :: (a -> Bool) -> List a -> List a
filterL p Empty = Empty
filterL p (Cons x xs)
  | p x = Cons x (filterL p xs)
  | otherwise = filterL p xs
filterL p (VList vl) = VList $ vl >>= (pure . rev)

vfilter :: (a -> Bool) -> VList a -> VList a
vfilter p vs = (vs >>= (pure . rev))
