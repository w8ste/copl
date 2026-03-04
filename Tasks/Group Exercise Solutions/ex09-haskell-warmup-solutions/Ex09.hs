module Ex09 where

-- Task 3

fact :: Integral a => a -> a
fact 0 = 1
fact n = n * fact (n - 1)


map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : (map' f xs)

fold' :: (a -> v -> v) -> v -> [a] -> v
fold' f init [] = init
fold' f init (x:xs) = f x (fold' f init xs)


-- Task 4

data Shape = Circle Double | Rectangle Double Double deriving (Show)


area :: Shape -> Double
area (Circle r) = r * r * pi
area (Rectangle a b) = a * b


data BinTree a = Leaf | Node (BinTree a) a (BinTree a)

height :: (BinTree a) -> Int
height Leaf = 1
height (Node lhs v rhs) = (max (height lhs) (height rhs)) + 1









