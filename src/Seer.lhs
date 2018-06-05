The Blind Seer algorithm is a tree traversal that the client and server perform
together via MPC.

The labels of the tree are, in truth, sets of (Key, Value) pairs. We also
include an identifier for each node.

> {-# LANGUAGE LambdaCase #-}
> module Seer where

> import           Control.Monad.Writer
> import qualified Data.Map as M
> import           Formula
> import           Formula.Z3

> import           DB

As a first approximation, we just set the values in the database to be
enumerable. We can therefore make a simplifying assumption that all values are
integers.

> type Query = Expr

The model of the seer algorithm we have here reveals a tree whose leaves are
either labels (early termination) or records.

> seer :: DB -> Query -> IO (Tree () (Maybe Record))
> seer (Leaf r) q = 
>   isSat (q `mkAnd` formulate r) >>= \case
>     True -> pure $ Leaf (Just r)
>     False -> pure $ Leaf Nothing
> seer (Branch lbl left right) q =
>   isSat (q `mkAnd` lbl) >>= \case
>     True -> Branch () <$> seer left q <*> seer right q
>     False -> pure $ Leaf Nothing

The client is free to view the output of `seer'. By inspecting this output, the
client learns new facts about the labels. They can use this to augment their
own virtual database.

There are two sources of new information.
First, for each label of the database we know that the query holds or does not
hold. We append this information.
Second, we append information based on any records which are reached. The
records may contain information which can inform other queries.

> type VLabel = Expr
> type VDB = Tree VLabel Record

`augment` performs the function of updating a virtual database to reflect the
result of learning information from the query and corresopnding traversal.

> augment :: Query -> Tree () (Maybe Record) -> VDB -> VDB
> augment q traversal vdb = fst $ runWriter (augment' q traversal vdb)

We declare an auxilliary function `augment'` which maintains a `Writer`
context. This context holds additional facts which have been learned from new
record information at the leaves.

> augment' :: Query -> Tree () (Maybe Record) -> VDB -> Writer Disjunctive VDB
> -- The query provided new record information which we can add to our virtual
> -- database. Also, the formula which describes this record is propogated up to
> -- all parents.
> augment' _ (Leaf (Just r)) (Leaf r') = do
>   tell (Disjunctive $ formulate r)
>   pure (Leaf (M.union r r'))
> -- The query failed, so attach `not q` to all labels here and below.
> augment' q (Leaf Nothing) vdb =
>   pure (propagateFact (mkNot q) vdb)
> -- The general case where the query succeeds at a branch. First, proceed down
> -- both children. The new fact at this branch is composed from (1) the old fact,
> -- (2) the query, and (3) the facts learned by any records reached through the
> -- children.
> augment' q (Branch () l1 r1) (Branch lbl' l2 r2) = do
>   (l', Disjunctive lphi) <- listen (augment' q l1 l2)
>   (r', Disjunctive rphi) <- listen (augment' q r1 r2)
>   pure (Branch ((lbl' `mkOr` q) `mkOr` lphi `mkOr` rphi) l' r')
> -- Since it is assumed that the virtual database and the actual database have
> -- the same shape, the traversal must be a subtree of the virtual database.
> -- Under this assumption, the only possible interactions have already been
> -- described.
> augment' _ _ _ = impossible

`augment'` makes use of this helper function `propagateFact`, which just attaches
a formula to all labels in the given subtree.

> propagateFact :: Expr -> VDB -> VDB
> propagateFact _ (Leaf r) = Leaf r
> propagateFact phi (Branch phi' l r) =
>   Branch (phi `mkAnd` phi') (propagateFact phi l) (propagateFact phi r)

With this in place, the user need only start with an initial guess for the
virtual database. The initial guess is based off the shape of the actual
database (which we assume to be known).

> emptyVDB :: Tree a b -> VDB
> -- Our starting assumption for the leaves is they are empty records.
> emptyVDB (Leaf _) = Leaf M.empty
> -- Our starting assumption for the branches is that we know nothing (true).
> emptyVDB (Branch _ l r) = Branch (LBool False) (emptyVDB l) (emptyVDB r)

Then, they can call augment with each new traversal they find. Finally, they
can run a virtual version of the seer algorithm to try to answer queries.

When running the virtual version, the leaves of the tree have three possibilities:
First, the leaf might hold a record. The remaining cases account for different
reasons the virtual algorithm might terminate early:
The known fact about a label means the query does not hold. This reflects an
accurate termination at this leaf (running the same query on the actual database
would also terminate here).
The known fact about a label is insufficient to make a decision about whether or
not the query holds at a label. In this case, we terminate early. It is possible
that in the actual database the query would hold, but we are not in a position to
decide whether or not this is true.

Interestingly, the first two cases are reflected as possibilities in the
original seer algorithm whereas the final case is the new possibility.

> data VResult
>   = Result Record
>   | NotHolds
>   | Unknown
>   deriving (Show, Read, Eq, Ord)

The tree produced by running this virtual version of the seer algorithm

> vseer :: VDB -> Query -> IO (Tree () VResult)
> vseer (Leaf r) q =
>   entails (formulate r) q >>= \case
>     True -> pure $ Leaf (Result r)
>     False -> entails (formulate r) (mkNot q) >>= \case
>       True -> pure $ Leaf NotHolds
>       False -> pure $ Leaf Unknown
> vseer (Branch phi left right) q =
>   isSat (phi `mkAnd` q) >>= \case
>     False -> isValid (phi `mkAnd` mkNot q) >>= \case
>       True -> pure $ Leaf NotHolds
>       False -> pure $ Leaf Unknown
>     True -> Branch () <$> vseer left q <*> vseer right q

Some helper definitions follow:

> newtype Disjunctive = Disjunctive { getDisjunctive :: Expr }

> instance Monoid Disjunctive where
>   mempty = Disjunctive (LBool False)
>   mappend (Disjunctive x) (Disjunctive y) = Disjunctive $ mkOr x y

> impossible :: a
> impossible = error "this case should be impossible"
