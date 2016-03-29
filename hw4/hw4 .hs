{---------------------
Please note the following warning can be ignored (as per TA instructions on Piazza): 
    -XOverlappingInstances is deprecated: instead use per-instance pragmas OVERLAPPING/OVERLAPPABLE/OVERLAPS
----------------------}

-- Question 1
{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}
import System.Random
import Data.Int
import Data.List ((\\))

class (Show a) => Gen a where
    gen :: IO a

instance (Show a, Random a) => Gen a where
    gen = randomIO

instance (Gen a, Gen b) => Gen (a, b) where
    gen = do x <- gen 
             y <- gen 
             return (x,y) --return tuple 

instance (Gen a) => Gen [a] where
    gen = do len <- randomRIO(1,10) :: IO Int
             sequence ([gen | _ <- [1..len]]) --Sequence turns a list of IO actions to an IO action with a list.
                                              --The list comprehension creates a list from size 1 to random number 
                                              --'len' with a random number in each index.  
{- 
Original questions 2 and 3 below for reference purposes (as the were modified to answer question 4) 

-- Question 2 
class Testable a where
  test :: a -> IO Bool

instance Testable Bool where
  test b = return b

instance (Gen a, Testable b) => Testable (a -> b) where
  test t = do n <- gen 
              test(t n)

-- Question 3
quickCheck :: (Testable a) => Int -> a -> IO ()
quickCheck 0 _ = return ()
quickCheck n t = do b <- test t
                    if not b
                        then putStrLn (id "Failing") --suppress quotes 
                    else quickCheck (n-1) t
-}

-- Question 4 (utilizing modified question 2 & 3 as specified, originals above)
class Testable a where
  test :: a -> IO (Bool, String) 

instance Testable Bool where
  test b = return (b, "")

instance (Gen a, Testable b) => Testable (a -> b) where
  test t = do 
            n <- gen -- generate random number & assign to n
            (boolResult, tResult) <- test(t n) -- store return values
            let fResult = show n ++ " " ++ tResult  -- prepend new random number
            return (boolResult, fResult)
              
quickCheck :: (Testable a) => Int -> a -> IO ()
quickCheck 0 _ = return ()
quickCheck n t = do (b, s) <- test t
                    if not b
                        -- if test fails print failing results, else repeat 
                        then putStrLn (id "Failing Inputs = " ++ s) --suppress quotes 
                    else quickCheck (n-1) t

-- Question 5
isort :: [Int8] -> [Int8]
isort [] = []
isort (x:xs) = insert (isort xs)
  where insert [] = [x]
        insert (h:t) | x > h = h:insert t
                     | x <= h = h:x:t

qsort :: [Int8] -> [Int8]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a < x] ++ [x] ++ qsort [a | a <- xs, a > x]

-- Check the various properties of a sorted list (testbed includes order, reapplying sort, keeping the same length and list integrity)
testSort :: ([Int8] -> [Int8]) -> [Int8] -> Bool
testSort sort lst = orderChecker sortToTest  && (reSortChecker sort lst) && (numberOfElementsChecker (sortToTest) lst) && (listIntegChecker (sortToTest) lst) 
                    where sortToTest = sort lst

-- Firstly, the list should be in increasingly sorted order  
orderChecker :: [Int8] -> Bool
orderChecker (x:y:ys) = x <= y && orderChecker(y:ys)
orderChecker _ = True

-- Additionally, applying the sort again should not change the results
reSortChecker :: ([Int8] -> [Int8]) -> [Int8] -> Bool 
reSortChecker sort xs = sort (sort xs) == (sort xs)

-- Importantly, the number of elements should match the original list (e.g. duplicates shouldn't be lost)
numberOfElementsChecker :: [Int8] -> [Int8] -> Bool
numberOfElementsChecker xs bs = length xs == length bs

-- Lastly, we check for list integrity
-- The (\\) function is used to compute set difference. We find the set difference both ways, because the sets might have uneven sizes, and make sure the resulting length is an empty list.
-- This is done to confirm that the (bag) set of elements in list xs, which are not elements of list bs is the empty list. 
listIntegChecker :: [Int8] -> [Int8] -> Bool
listIntegChecker xs bs = length (xs \\ bs) == 0 && length (bs \\ xs) == 0

-- Question 6 
-- Please note isort' (isort prime) is the fixed isort, the original isort above is unmodified 
-- Please note qsort' (qsort prime) is the fixed qsort, the original qsort above is unmodified 

isort' :: [Int8] -> [Int8]
isort' [] = []
isort' (x:xs) = insert (isort' xs)
  where insert [] = [x]
        insert (h:t) | x > h = h:insert t
                     | x <= h = x:h:t -- Here the prepend order was incorrect 

qsort' :: [Int8] -> [Int8]
qsort' [] = []
qsort' (x:xs) = qsort' [a | a <- xs, a <= x] ++ [x] ++ qsort' [a | a <- xs, a > x] -- the <= between a & x on the left (i.e. a <= x) fixes qsort
