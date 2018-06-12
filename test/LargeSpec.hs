{-# LANGUAGE QuasiQuotes #-}

import           Control.Monad
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           DB
import           Formula
import           Formula.Z3
import           Seer
import           System.Random
import           Util

import           Data.Text.Prettyprint.Doc

numDistinct = 100

numRecords = 10000

rdb :: IO DB
rdb = mkDB <$> replicateM numRecords rRecord

rRecord :: IO Record
rRecord = do
  a' <- randomRIO (0, numDistinct)
  b' <- randomRIO (0, numDistinct)
  pure (M.fromList [(a, a'), (b, b')])

a, b :: Var
a = Var "a" Int

b = Var "b" Int

q1, q2, q3 :: Query
q1 = [expr|a = 1|]
q2 = [expr|b = 3|]
q3 = [expr|a = 1 && b = 3|]

main :: IO ()
main = do
  db <- rdb
  t1 <- seer db q1
  putStrLn "here"
  t2 <- seer db q2
  putStrLn "there"

  let model = augment q2 t2 (augment q1 t1 (emptyVDB db))
  putStr =<< depictRes <$> vseer model q3
