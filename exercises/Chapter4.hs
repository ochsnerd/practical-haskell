{-# LANGUAGE TupleSections #-}

module Chapter4 where

import Client
import Data.Function (on)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S

-- Altering your map
insertA :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
insertA key value = M.alter (const $ Just value) key

-- point free or die
insertA' :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
insertA' = (flip $ dove $ flip M.alter) (const . return)
  where
    dove :: (a -> c -> d) -> a -> (b -> c) -> b -> d
    dove f x g y = f x (g y)

deleteA :: (Ord k) => k -> M.Map k a -> M.Map k a
deleteA = M.alter (const Nothing)

adjustA :: (Ord k) => (a -> a) -> k -> M.Map k a -> M.Map k a
adjustA = M.alter . fmap

-- Classifying Clients
data ClientKind = IndividualKind | CompanyKind | GovOrgKind deriving (Enum, Bounded, Eq, Ord, Show)

kind :: Client a -> ClientKind
kind (GovOrg {}) = GovOrgKind
kind (Company {}) = CompanyKind
kind (Individual {}) = IndividualKind

classifyClients :: (Ord a) => [Client a] -> M.Map ClientKind (S.Set (Client a))
classifyClients = foldr classify empty
  where
    -- start with an empty set for every kind to make insertion uniform
    empty = M.fromList . fmap (,S.empty) $ [minBound ..]
    -- classify c = M.adjust (S.insert c) (kind c)
    classify = liftA2 M.adjust S.insert kind

-- note: Using Set for Client is problematic because
-- - Set requires Ord
-- - implementing Ord for Client requires Ord for its id type
-- - Ord for an id is problematic
--   (ids should only Eq and Hash)

-- Prices for the store
-- representing money: https://stackoverflow.com/a/27410389
-- fixed point arithmetic: https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Fixed.html
class Priceable p where
  price :: p -> Double

totalPrice :: (Priceable p) => [p] -> Double
totalPrice = sum . fmap price

-- The same Client
data Client' id
  = GovOrg' {clientId :: id, clientName :: String}
  | Company'
      { clientId :: id,
        clientName :: String,
        person :: Person,
        duty :: String
      }
  | Individual' {clientId :: id, person :: Person}
  deriving (Show)

instance (Eq a) => Eq (Client' a) where
  (GovOrg' id_l clientName_l) == (GovOrg' id_r clientName_r) = id_l == id_r && clientName_l == clientName_r
  -- TODO: the rest
  _ == _ = False

name :: Client' a -> String
name (GovOrg' _ n) = n
name (Company' _ n _ _) = n
name (Individual' _ (Person fn ln)) = ln <> ", " <> fn

kind' :: Client' a -> ClientKind
kind' (GovOrg' {}) = GovOrgKind
kind' (Company' {}) = CompanyKind
kind' (Individual' {}) = IndividualKind

instance (Ord a) => Ord (Client' a) where
  compare c1 c2 = case (compare `on` name) c1 c2 of
    EQ -> case (compare `on` kind') c1 c2 of
      EQ -> compare' c1 c2
      c -> c
    c -> c
    where
      -- this isn't very typesafe...
      compare' (GovOrg' id_l _) (GovOrg' id_r _) = compare id_l id_r
      compare' (Company' {}) (Company' {}) = undefined -- TODO: blabla
      compare' (Individual' id_l _) (Individual' id_r _) = compare id_l id_r
      compare' _ _ = undefined

-- More Operations on Generic Trees
data BinaryTree2 a
  = Node2 a (BinaryTree2 a) (BinaryTree2 a)
  | Leaf2
  deriving (Show)

-- this is 'return'
singleton :: a -> BinaryTree2 a
singleton v = Node2 v Leaf2 Leaf2

treeInsert :: (Ord a) => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert t Leaf2 = singleton t
treeInsert t n@(Node2 v l r) = case compare t v of
  EQ -> n
  LT -> Node2 v (treeInsert t l) r
  GT -> Node2 v l (treeInsert t r)

fromList :: (Ord a) => [a] -> BinaryTree2 a
fromList = foldr treeInsert Leaf2

combine :: (Ord a) => BinaryTree2 a -> BinaryTree2 a -> BinaryTree2 a
combine Leaf2 = id
combine (Node2 v l r) = treeInsert v . combine r . combine l

-- dubious name
concat :: (Ord a) => [BinaryTree2 a] -> BinaryTree2 a
concat = foldl' combine Leaf2

-- Functor Fun
data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' $ f x

instance Functor BinaryTree2 where
  fmap _ Leaf2 = Leaf2
  -- note that this can lead to a 'broken' tree, for example by `fmap negate`,
  -- but to restore the invariant after applying f, we'd need `b` to be Ord (which we can't enforce)
  fmap f (Node2 v l r) = Node2 (f v) (fmap f l) (fmap f r)

-- Foldable Fun
instance Foldable Maybe' where
  foldMap _ Nothing' = mempty
  foldMap f (Just' x) = f x

instance Foldable BinaryTree2 where
  foldMap _ Leaf2 = mempty
  -- here we decide in which order to fold, this is preorder (like Data.Tree)
  foldMap f (Node2 v l r) = f v <> foldMap f l <> foldMap f r
