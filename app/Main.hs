{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Var
import VarList

import Data.Data
import Data.Typeable

data Food = Steak | Pasta | Fries | Cake
  deriving (Eq, Show, Data, Typeable)

type Menu = VList Food

dessert :: Menu
dessert = atomic "Dessert" ["yes","no"] [vsingle Cake, vempty]

menu :: Menu
menu = atomic "Main" ["meat","pasta"]
              [vlist [Steak, Fries], Pasta `vcons` dessert]

menu' :: Menu
menu' = atomic "Main" ["meat","pasta"]
               [vlist [Steak, Fries], Pasta `vcons` dessert]

main :: IO ()
main = putStrLn "Hello world"
