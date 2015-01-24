module Data.ParTree
(
	emptyTree,
	fromListToNode
)where

import Control.Monad
import Data.Functor
import Data.Monoid
import Control.Applicative
import qualified Data.List  as L
import qualified Data.Map   as M

import Control.Concurrent.STM

--Implementation of Parallel generic tree
--Need to append levels on this tree


type Levels a = a
--data Node a = EmptyNode | Tnode a deriving Show
data Tree a= Empty | Node a | NodeList (a, [Tree a]) deriving Show

instance Functor Tree where
	fmap f (Node value) = Node (f value)

instance Monoid (Tree v) where
	mempty = Empty
	Node (value) `mappend` Node (value2) = Node (value)

emptyTree :: Tree a
emptyTree = Empty

--Construct tree from List
--For example: [(4, [5,7])]
fromListToNode:: [(a, [a])] -> Tree a
fromListToNode [] = Empty
fromListToNode items = NodeList (fst $ head items, map(\x -> Node x) (snd $ head items))

insertData :: a -> Tree [a] -> Tree [a]
insertData value (Node alldata) = Node(value: alldata)

insertWithTree:: Tree a -> Tree a -> Tree a
insertWithTree (Empty) (Node value) = NodeList (value, [Empty])
insertWithTree (Node value) (Node value2) = NodeList (value, [Node value2])
insertWithTree (NodeList (value, listofnodes)) (Node value2) = NodeList (value, (Node value2): listofnodes)

constructTree:: Functor f => (a -> f b) -> a -> (Tree (f b))
constructTree func value = (Node (func value))

filterTree:: Tree [a] -> (a -> Bool) -> Tree [a]
filterTree Empty _= Empty
filterTree (Node value) func = Node (filter func value)

--find:: Tree [a] -> a -> Bool
--find tree findvalue = case tree of Empty -> False
--	      					(Node value) -> True

--equal:: Tree a -> Tree a -> Bool
--equal (Node value) (Node value2) = value == value2

--TODO: flatten