{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Var
import Data.Var.VarList

import Data.Data
import Data.Typeable

data Food = Steak | Pasta | Fries | Cake | Sherry
  deriving (Eq, Show, Data, Typeable)

type Menu = VList Food

dessert = opt "Dessert" Cake
meat  = "meat"  <: vlist [Steak, Fries]
pasta = "pasta" <: Pasta `vcons` dessert
menu  = alt "Main" [meat, pasta]

aperitif :: VList Food
aperitif = opt "Drink" Sherry

main :: IO ()
main = print menu
