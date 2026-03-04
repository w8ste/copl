module Ex10 where
-- Task 1
-- list containing only 1
ones = 1 : ones

-- list of natural numbers
nats = 0 : map (+ 1) nats -- [0,1..]

-- list of squares
squares = map (^ 2) nats -- [x^2 | x <- nats]

-- list of fibonacci numbers
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- if
myif :: Bool -> a -> a -> a
myif cond thn els
  | cond = thn
  | otherwise = els


-- Task 2
data Option a = None | Some a -- deriving (Show)

data NonEmptyList a = Elem a | Many a (NonEmptyList a)

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)


-- Task 3
-- a)
double x = 2 * x

-- b)
instance Show a => Show (Option a) where
	show None = "None"
	show (Some v) = "Some (" ++ (show v) ++ ")"

instance Show a => Show (NonEmptyList a) where
    show (Elem v)  = show v
    show (Many x xs) = (show x) ++ " : " ++ (show xs)

instance Show a => Show (BinTree a) where
  show (Leaf v)  = "Leaf " ++ (show v)
  show (Node l r) = "Node (" ++ (show l) ++ ") (" ++ (show r) ++ ")"

instance Eq a => Eq (Option a) where
  (==) None None = True
  (==) (Some a) (Some b) = a == b
  (==) _ _ = False

instance Eq a => Eq (NonEmptyList a) where
  (==) (Elem a) (Elem b) = a == b
  (==) (Many a as) (Many b bs) = a == b && as == bs
  (==) _ _ = False

instance Eq a => Eq (BinTree a) where
  (==) (Leaf l) (Leaf r) = l == r
  (==) (Node xl xr) (Node yl yr) = xl == yl && xr == yr
  (==) l r = False


  -- c)
class Addable f where
	create :: a -> f a
	grow :: a -> f a -> f a

	-- You can use fromList by specifying the expected type, e.g., y = (fromList [2,1,3]) :: Tree Int
	fromList :: [a] -> f a
	fromList (x : []) = create x
	fromList (x : xs) = grow x (fromList xs)

instance Addable NonEmptyList where
	create v = Elem v
	grow v l = Many v l

instance Addable BinTree where
  create v = Leaf v
  grow v tree = Node (Leaf v) tree

instance Addable [] where
	create v  = [v]
	grow v list = v : list

-- We can not implement an instance of Addable for type Int.
-- Addable expects a type f, which takes a type parameter itself (see f a in, e.g., the type
-- signature of grow). Integer does not take a type parameter.
-- Therefore, we can not implement Addable for integers.

-- We can not sensibly implement an instance of Addable for option, as
-- it is not clear what should happen when we grow an Option.
instance Addable Option where
  create v = Some v
  grow v opt = Some v
