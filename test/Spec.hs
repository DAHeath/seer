{-# LANGUAGE QuasiQuotes #-}
import           Data.Map   (Map)
import qualified Data.Map   as M
import           DB
import           Formula
import           Formula.Z3
import           Seer

import Data.Text.Prettyprint.Doc

db :: DB
db =
  mkDB
    [ M.fromList [(a, 1), (b, 2)]
    , M.fromList [(a, 1), (b, 3)]
    , M.fromList [(a, 2), (b, 3)]
    , M.fromList [(a, 3), (b, 4)]
    ]
  where
    a = Var "a" Int
    b = Var "b" Int

q1, q2, q3 :: Query
q1 = [expr|a = 1|]
q2 = [expr|b = 3|]
q3 = [expr|a = 1 && b = 3|]

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

main :: IO ()
main = do
  t1 <- seer db q1
  t2 <- seer db q2

  let model = augment q2 t2 (augment q1 t1 (emptyVDB db))
  putStr $ depictDB model
  putStr =<< depictRes <$> vseer model q3
