The Blind Seer algorithm is a tree traversal that the client and server perform
together via MPC.

The labels of the tree are, in truth, sets of (Key, Value) pairs. We also
include an identifier for each node.

> import Data.Set

> data Formula
> data Key
> data Value
> data Record
> data ID
> type Label = Set (Key, Value)
> type Query = Formula -- perhaps something more specific?
>
> data Tree a b = Branch a (Tree a b) (Tree a b) | Leaf b
> type DB = Tree Label Record

The model of the seer algorithm we have here reveals a tree whose leaves are
either labels (early termination) or records.

> seer :: DB -> Query -> Tree () (Maybe Record)
> seer (Leaf r) _ = Leaf (Just r)
> seer (Branch lbl left right) q
>   | lbl `satisfies` q = Branch () (seer left q) (seer right q)
>   | otherwise = Leaf Nothing
>
> satisfies :: Label -> Query -> Bool
> satisfies = undefined

The client is free to view the output of `seer'. By inspecting the output, the
client learns new facts about the labels. They can use this to augment their
own virtual database:

> type VLabel = Formula
> type VDB = Tree VLabel (Either Formula Record)

> augment :: Query -> Tree () (Maybe Record) -> VDB -> VDB
> -- The query failed here
> augment q (Leaf Nothing) (Leaf (Left phi)) = Leaf (Left (phi `land` lnot q))
> -- The query did not get to the record
> augment q (Leaf Nothing) (Leaf (Right r)) = Leaf (Right r)
> -- The query provided a new record for the virtual database
> augment _ (Leaf (Just r)) _ = Leaf (Right r)
> -- The query failed at this branch
> augment q (Leaf Nothing) (Branch lbl left right) =
>   Branch (lbl `land` lnot q) left right
> -- The query succeeded at this branch
> augment q (Branch () left right) (Branch lbl' left' right') =
>   Branch (lbl' `land` q) (augment q left left') (augment q right right')
> -- The query revealed a new branch which is not in the virtual database yet
> augment q (Branch () left right) (Leaf (Left phi)) =
>   Branch (phi `land` q) (augment q left (Leaf (Left ltrue)))
>                         (augment q right (Leaf (Left ltrue)))
>
> -- Logical conjunction
> land :: Formula -> Formula -> Formula
> land = undefined
>
> -- Logical negation
> lnot :: Formula -> Formula
> lnot = undefined
>
> -- The formula `true'
> ltrue :: Formula
> ltrue = undefined

With this in place, the user need only start with an initial guess for the
virtual database:

> emptyVDB :: VDB
> emptyVDB = Leaf (Left ltrue)

Then, they can call augment with each new traversal they find. Finally, they
can run a virtual version of the seer algorithm to try to answer queries.

NOTE: This algorithm is the biggest candidate for modification and extension:

> vseer :: VDB -> Query -> [Record]
> vseer (Leaf (Right r)) _ = [r]
> vseer (Leaf (Left _)) _ = []
> vseer (Branch lbl left right) q
>   | lsat (lbl `limpl` q) = vseer left q ++ vseer right q
>   | otherwise = []
>
> -- Boolean satisfiability
> lsat :: Formula -> Bool
> lsat = undefined
>
> -- Logical implication
> limpl :: Formula -> Formula -> Formula
> limpl = undefined
