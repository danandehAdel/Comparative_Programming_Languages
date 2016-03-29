
-- Question 1
myFoldl :: (a-> b-> a) ->a -> [b] -> a
myFoldl opr base [] = base
myFoldl opr base (x:xs) = myFoldl opr (opr base x) xs

-- Question 2
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) [] 

-- Question 3
-- id = identity function x = x
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f a xs = foldl(\h b x -> h (f b x)) id xs a

-- Question 4
-- id = identity function x = x
myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f a xs = foldr (\b h x -> h (f x b)) id xs a

-- Question 5: 
isUpper :: Char -> Bool
isUpper input = input `elem` ['A'..'Z']

-- Question 6: 
onlyCapitals1 :: String -> String
onlyCapitals1 input = filter (isUpper) input

-- Question 7: 
onlyCapitals2 :: String -> String
onlyCapitals2 input = [ x | x <- input, isUpper x]  

-- Question 8: 
onlyCapitals3 :: String -> String
onlyCapitals3 [] = []
onlyCapitals3 (x:xs) 
    |isUpper x = x: onlyCapitals3 xs
    |otherwise = onlyCapitals3 xs

-- Question 9
divRemainder :: Int -> Int -> (Int, Int)
divRemainder a b = (a `div` b , a `mod` b)

-- Question 10
digitSum :: Int -> Int
digitSum 0 = 0
digitSum x = (x `mod` 10) + digitSum (x `div` 10)
