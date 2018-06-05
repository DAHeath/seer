> {-# LANGUAGE QuasiQuotes        #-}
> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE DeriveFunctor      #-}
> {-# LANGUAGE DeriveTraversable  #-}
> module DB where

> import           Data.Data (Data)
> import           Data.Map (Map)
> import qualified Data.Map as M
> import           Formula

> type Key = Var

> type Label = Expr
> type Value = Integer
> type Record = Map Key Value

> data Tree a b = Branch a (Tree a b) (Tree a b) | Leaf b
>   deriving (Show, Read, Eq, Ord, Data, Functor, Foldable, Traversable)
> type DB = Tree Label Record

To model Blind Seer we need a way to build a database. Given a list of records
`mkDB` constructs a tree database with appropriate labels.

> mkDB :: [Record] -> DB
> mkDB [] = Leaf M.empty
> mkDB [r] = Leaf r
> mkDB rs =
>   let len = length rs
>       l = mkDB (take (len `div` 2) rs) 
>       r = mkDB (drop (len `div` 2) rs)
>       lFact = dbFact l
>       rFact = dbFact r
>   in Branch (lFact `mkOr` rFact) l r
>   where
>     dbFact (Leaf r) = formulate r
>     dbFact (Branch phi _ _) = phi

We declare a function `formulate` which converts a record to a formula.

> formulate :: Record -> Expr
> formulate =
>   foldr (mkAnd . (\(k, v) ->
>     let i = LInt v in [expr|@k = $i|])) (LBool True) . M.toList
