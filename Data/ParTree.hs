module Data.ParTree
(
	emptyTree,
	fromListToNode
)where

import Control.Monad
import Data.Functor
import Control.Applicative
import qualified Data.List  as L
import qualified Data.Map   as M

import Control.Concurrent.STM

--Implementation of Parallel generic tree
--Need to append levels on this tree

type Levels a = a
data Tree a= Empty | Node a deriving Show

instance Functor Tree where
	fmap f (Node value) = Node (f value)

emptyTree :: Tree a
emptyTree = Empty

--Construct tree from List
--For example: [4,5,6,7]
fromListToNode:: [a] -> Tree [a]
fromListToNode [] = Empty
fromListToNode items = Node items

insertData :: a -> Tree [a] -> Tree [a]
insertData value (Node alldata) = Node(value: alldata)

constructTree:: Functor f => (a -> f b) -> a -> (Tree (f b))
constructTree func value = (Node (func value))

filterTree:: Tree [a] -> (a -> Bool) -> Tree [a]
filterTree Empty _= Empty
filterTree (Node value) func = Node (filter func value)

--TODO: flatten