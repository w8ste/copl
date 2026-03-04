module Assignment where

-- The State monad (introduced in the lecture) can be used for propagating a
-- counter, which can in turn be used to collect certain statistical information
-- about the computation.
-- 
-- Define two operations useful when working with a counter:
--
--   - incr : increases the current counter value by one
-- 
--   - runWithCounter : runs the given computation with a counter, initalized to
--                      0, and returns a pair consisting of the computation
--                      result and the counter's value at the end of the
--                      computation.

-- Here are the state transformer definitions from the lecture (note that slight
-- changes of names, from StateT to State):
--
-- A state transformer, where s is the type of the state and a is
-- the result type

data State s a = State (s -> (a, s))


-- Note, at some point the Haskell standard library changed, such that instances of Monad now require instances of Functor and Applicative.
-- This has reasons that make sense within the standar library, but are mostly just annoying for our lecture.
-- We do provide the instances of Functor and Applicative, though they are not used for this program (we don’t use any of the functions of Monad that would make use of them)

instance Functor (State s) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (State af) =
      State(\s ->
        let (a, s') = (af s)
        in ((f a), s')
      )

instance Applicative (State s) where
  pure a = State(\s -> (a, s))
  --(<*>) :: (State s (a -> b)) -> (State s a) -> (State s b)
  State(l) <*> State(r) =
    State(\s ->
      let (f, s')  = (l s)
          (v, s'') = (r s')
      in ((f v), s'')
    )

-- Note: `return` in Monad is now an alias for `pure` in Applicative, thus no longer needs to be defined explicitly
instance Monad (State s) where
  -- (>>=) :: (State s a) -> (a -> State s b) -> (State s b)
  (State st) >>= k =
    State (\s ->
      let (value, s') = st s
          (State t) = k value
      in t s'
    )

-- the pattern match in the interpreter below may fail, in which case we delegate the failure to Haskells built in ”error” which aborts the program. (2 points)
instance MonadFail (State s) where
  fail = error

incr :: State Int ()
incr = undefined

runWithCounter :: State Int a -> (a, Int)
runWithCounter = undefined

-- Below is a test case that the counter to count the number of comparisions
-- during a merge sort.

merge xs [] = return xs
merge [] ys = return ys
merge (x:xs) (y:ys) = do
  incr
  if x < y
  then do { zs <- merge xs (y:ys); return (x:zs) }
  else do { zs <- merge (x:xs) ys; return (y:zs) }

mSort [] = return []
mSort [x] = return [x]
mSort l =
  do let len = length l
     s1 <- mSort (take (len `div` 2) l)
     s2 <- mSort (drop (len `div` 2) l)
     merge s1 s2

test5 = [ runWithCounter (mSort [3, 2, 1, 5]) == ([1, 2, 3, 5], 5) ]
