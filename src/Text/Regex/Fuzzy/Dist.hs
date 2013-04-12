-- | This module implements reasonably fast incremental levenshtein distance
--   algorithm in pure functional fashion.
--
--   We use funny data structure which carry on just the front of the table
--   used to cache previously computed distances. Therefore we have low memory
--   consumption suitable for considerable long sequences - /O(n + m)/ memory
--   space where /n/, /m/ are length of right and left sequences respectively.
--   However the structure let us find distances incrementally, so we'll have
--   to expand the table either on the left or on the right in any moment.
--
--   Suppose the base case - we have both sequences empty. It'll look just like:
--
--   @
--     x
--   @
--
--   Next we insert an element to the right:
--
--   @
--     a x
--   @
--
--   And to the left:
--
--   @
--       k
--     a x
--   @
--
--   After a few insertions we can get something like:
--
--   @
--           k
--           l
--     a b c x
--   @
--
--   Now suppose we want to insert to the right so we can see how insertion is done:
--
--     * At first we need to move non-main front at one position.
--
--     @
--             k
--             l
--     a b c x
--     @
--
--     * At second we need to insert new element to the main front.
--
--     @
--             k
--             l
--     a b c d
--     @
--
--     * Finally we should just find overall edit distance 'x'.
--
--     @
--             k
--             l
--     a b c d x
--     @
--
--   Where main front is where we want to insert to. Thanks for symmetry
--   we can implement insertion to the left by swapping fronts.
--   Insertion cost is /O(n)/ where the /n/ is length of sequence to insert to.
--
module Text.Regex.Fuzzy.Dist
       ( Dist, DistFront, DistTable

       -- * Query
       , editDist, leftSeq, rightSeq

       -- * Construction
       , emptyF, insertLeft, insertRight, insertBoth, insertMany
       , (<.), (.>)

       -- * Extra
       , findEditDist

       -- * Debug
       )
       where

-- | Amount of distance between two sequences of an arbitrary length.
type Dist = Int

-- | Accumulated so far cost for each element in the sequence.
type Front     a = [(Dist, a)]

-- | Zipper like structure which contain the left and right fronts and current
--   edit distance.
type DistFront a = (Front a, Dist, Front a)

-- | Accumulated distance fronts.
type DistTable a = [DistFront a]


-- | Get edit distance for the current position.
editDist :: DistFront a -> Dist
editDist (_, d, _) = d

-- | Gives left sequence which have been inserted by 'insertLeft'.
leftSeq :: DistFront a -> [a]
leftSeq (p, _, _) = map snd p

-- | Gives right sequence which have been inserted by 'insertRight'.
rightSeq :: DistFront a -> [a]
rightSeq (_, _, q) = map snd q

-- | Initial front's value.
emptyF :: DistFront a
emptyF = ([], 0, [])

-- | Exchange left and right front's.
swapFronts :: DistFront a -> DistFront a
swapFronts (p, x, q) = (q, x, p)


dist :: Dist -> Dist -> Dist -> Bool -> Dist
dist p x q b = minimum [succ p, succ q, x + fromEnum b]

moveFront :: Eq a
             => a       -- ^ Element of the other front at new front position.
             -> Int     -- ^ New position index.
             -> Front a -- ^ Arbitrary long front to move.
             -> Front a -- ^ Front moved at 1 pos in orth direction.
moveFront e j = go
  where
    go [] = []
    go ((d, x) : xs) = let xs' = go xs in
      (dist (lastDist xs') (lastDist xs) d (e /= x), x) : xs'

    lastDist [] = j
    lastDist ((d, _) : _) = d


insertFront :: Eq a => Dist -> a -> Front a -> Front a
insertFront d e fr = (d, e) : fr

insertLeft :: Eq a => a -> DistFront a -> DistFront a
insertLeft e (p, c, q) = (p', c', q')
  where
    p' = insertFront c e p
    c' = dist (lastDist (length q) p')
              (lastDist (length p) q)
              (lastDist (length p') q')
              (comp e q)
    q' = moveFront e (length p') q

    comp _  [] = True
    comp e' ((_, x) : _) = e' /= x

    lastDist j [] = j
    lastDist _ ((d, _) : _) = d

insertRight :: Eq a => DistFront a -> a -> DistFront a
insertRight df x = swapFronts (insertLeft x (swapFronts df))

insertBoth :: Eq a => a -> DistFront a -> a -> DistFront a
insertBoth a_nn d b_nn = a_nn `insertLeft` d `insertRight` b_nn

insertMany :: Eq a => [a] -> DistFront a -> [a] -> DistFront a
insertMany []       acc []       = acc
insertMany (x : xs) acc []       = insertMany xs (x   `insertLeft`  acc) []
insertMany []       acc (y : ys) = insertMany [] (acc `insertRight` y)   ys
insertMany (x : xs) acc (y : ys) = insertMany xs (insertBoth x acc y)    ys

-- | Operator version of 'insertLeft'.
(<.) :: Eq a => a -> DistFront a -> DistFront a
(<.) = insertLeft

infixr 3 <.

-- | Operator version of 'insertRight'.
(.>) :: Eq a => DistFront a -> a -> DistFront a
(.>) = insertRight

infixl 4 .>

findEditDist :: Eq a => [a] -> [a] -> Int
findEditDist a b = editDist (insertMany a emptyF b)
