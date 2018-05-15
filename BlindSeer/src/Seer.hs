module Seer where

import           Data.Tree

type Seer = Tree Content

data Record =
  Map Label

mkSeer :: [Record] -> Seer
mkSeer = undefined
