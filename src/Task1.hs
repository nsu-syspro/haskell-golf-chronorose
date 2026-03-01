module Task1 where

-- | Compresses given data using run-length encoding.
--
-- Usage example:
--
-- >>> encode "aaabbccaadaaa"
-- [(3,'a'),(2,'b'),(2,'c'),(2,'a'),(1,'d'),(3,'a')]
-- >>> encode "abc"
-- [(1,'a'),(1,'b'),(1,'c')]
-- >>> encode []
-- []
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x : xs) = go (1, x) xs
  where
    go (l, e) [] = [(l, e)]
    go (l, e) (y : ys) = if e == y then go (l + 1, e) ys else (l, e) : go (1, y) ys

-- | Decompresses given data using run-length decoding.
--
-- Usage example:
--
-- >>> decode [(3,'a'),(2,'b'),(2,'c'),(2,'a'),(1,'d'),(3,'a')]
-- "aaabbccaadaaa"
-- >>> decode [(1,'a'),(1,'b'),(1,'c')]
-- "abc"
-- >>> decode []
-- []
decode :: [(Int, a)] -> [a]
decode = concatMap $ uncurry replicate

-- | Rotates given finite list to the left for a given amount N
--
-- If N is negative, then rotates to the right instead.
--
-- Usage example:
--
-- >>> rotate 3 "abcdefgh"
-- "defghabc"
-- >>> rotate (-2) "abcdefgh"
-- "ghabcdef"
-- >>> rotate 0 "abcdefgh"
-- "abcdefgh"
-- >>> rotate 5 "abc"
-- "cab"
-- >>> rotate 5 ""
-- ""
rotate :: Int -> [a] -> [a]
rotate x y = take n . drop (x `mod` n) . cycle $ y
  where
    n = length y
