{-# LANGUAGE LambdaCase #-}

module Chapter3 where

import Data.List (find, foldl')
import Data.Maybe (isJust)

-- 3.1: A new life as a type checker
swapTriple :: (c, a, b) -> (a, b, c)
swapTriple (x, y, z) = (y, z, x)

duplicate :: a -> (a, a)
duplicate x = (x, x)

nothing :: b -> Maybe a
nothing _ = Nothing

index :: (Num a) => [b] -> [(a, b)]
index [] = []
index [x] = [(0, x)]
index (x : xs) =
  let indexed@((n, _) : _) = index xs
   in (n + 1, x) : indexed

maybeA :: [a] -> Char
maybeA [] = 'a'

-- 3.2 Working with filters
filterOnes :: (Num a, Eq a) => [a] -> [a]
filterOnes = filter (== 1)

filterANumber :: (Eq a) => a -> [a] -> [a]
filterANumber n = filter (== n)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = filter (not . p)

data Client id
  = GovOrg {clientId :: id, clientName :: String}
  | Company
      { clientId :: id,
        clientName :: String,
        person :: Person,
        duty :: String
      }
  | Individual {clientId :: id, person :: Person}
  deriving (Show, Eq, Ord)

data Person = Person {firstName :: String, lastName :: String}
  deriving (Show, Eq, Ord)

filterGovOrgs1 :: [Client a] -> [Client a]
filterGovOrgs1 = filter isGovOrg
  where
    isGovOrg (GovOrg _ _) = True
    isGovOrg _ = False

filterGovOrgs2 :: [Client a] -> [Client a]
filterGovOrgs2 =
  filter
    ( \case
        GovOrg _ _ -> True
        _ -> False
    )

-- 3.3 Your first folds
-- Hello Monoids
productR :: (Num a) => [a] -> a
productR [] = 1
productR (x : xs) = x * productR xs

-- (*) is commutative, foldr/foldl does not matter
productF :: (Num a) => [a] -> a
productF = foldl' (*) 1

minimumClientR :: [Client a] -> Maybe (Client a)
minimumClientR [] = Nothing
minimumClientR (c : cs) = Just (minimumClient' c cs)
  where
    minimumClient' c0 [] = c0
    minimumClient' c0 (c' : cs') = minimumClient' (minBy name c0 c') cs'

-- minBy fst (0, 1) (0, 0) == (0, 1) because thats what c++ does
minBy :: (Ord a) => (b -> a) -> b -> b -> b
minBy f x y = if f y < f x then y else x

-- is there some nicer syntax to use here?
name :: Client a -> String
name (GovOrg _ n) = n
name (Company _ n _ _) = n
name (Individual _ (Person _ ln)) = ln

-- min is commutative,
-- but if the minimum Client is not unique, foldr/foldl matters
-- (so I guess "min by key" is not commutative/associative?)

-- instead of returning maybe, we let foldl1 throw an error if the list is empty
minimumClientF :: [Client a] -> Client a
minimumClientF = foldl1 (minBy name)

-- && is again commutative
allR :: [Bool] -> Bool
allR [] = True -- thats what python does
allR (b : bs) = b && allR bs

allF :: [Bool] -> Bool
allF = foldl' (&&) True

minimumBy :: (Ord a) => (b -> a) -> [b] -> b
minimumBy g = foldl1 (minBy g)

myElem :: (Eq a) => a -> [a] -> Bool
myElem e = isJust . find (e ==)
