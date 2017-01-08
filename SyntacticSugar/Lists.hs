module Lists where

-- All four of these functions do the same thing
-- They have different levels of syntactic sugar

-- Do block with semicolons
v1 :: [a] -> [b] -> [c] -> [(a,b)]
v1 xs ys zs = do {
  x <- xs;
  y <- ys;
  zs;
  return (x,y);
}

-- Do block with whitespace
v2 :: [a] -> [b] -> [c] -> [(a, b)]
v2 xs ys zs = do
  x <- xs
  y <- ys
  zs
  return (x,y)

-- Explicit use of >>= and >>
v3 :: [a] -> [b] -> [c] -> [(a,b)]
v3 xs ys zs =
  xs >>= \x -> (ys >>= \y -> (zs >> return (x,y)))

-- List Comprehension
v4 :: [a] -> [b] -> [c] -> [(a,b)]
v4 xs ys zs =
  [(x, y) | x <- xs, y <- ys]

main :: IO ()
main =
  let l1 = (v1 [1..2] [1..3] [10])
      l2 = (v2 [1..2] [1..3] [10])
      l3 = (v3 [1..2] [1..3] [10])
      l4 = (v4 [1..2] [1..3] [10])
  in mapM_ (putStrLn . show) [l1, l2, l3, l4]
