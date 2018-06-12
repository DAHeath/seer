{-# LANGUAGE QuasiQuotes #-}
import           Data.Map   (Map)
import qualified Data.Map   as M
import           DB
import           Util
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

main :: IO ()
main = do
  t1 <- seer db q1
  t2 <- seer db q2

  putStr $ depictDB db
  putStrLn ""
  let model = augment q2 t2 (augment q1 t1 (emptyVDB db))
  putStr $ depictDB model
  putStr =<< depictRes <$> vseer model q3
