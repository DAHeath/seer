module Util where

import           Data.Map   (Map)
import qualified Data.Map   as M
import           DB
import           Formula
import           Formula.Z3
import           Seer

import Data.Text.Prettyprint.Doc

mapLabels :: (a -> b) -> Tree a c -> Tree b c
mapLabels _ (Leaf c) = Leaf c
mapLabels f (Branch lbl l r) = Branch (f lbl) (mapLabels f l) (mapLabels f r)

depictDB :: DB -> String
depictDB = depict (show . pretty) (show . pretty . M.toList)

depictRes :: Tree () VResult -> String
depictRes = depict show show

depict :: (a -> String) -> (b -> String) -> Tree a b -> String
depict sa sb = unlines . go
  where
    go (Leaf b) = [sb b]
    go (Branch lbl l r) =
      [ sa lbl ]
      ++ indent (go l)
      ++ indent (go r)
    indent = map ("  "++)
