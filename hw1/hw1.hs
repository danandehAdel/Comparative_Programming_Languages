--Homework1


--Number 1
citeAuthor :: String -> String -> String
citeAuthor firstname lastname = lastname ++ ", " ++ firstname


--Number 2
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname 


--Number 3
title :: (String, String, Int) -> String
title (author, bookName, year) = bookName


--Number 4
citeBook :: (String, String, Int) -> String
citeBook (author, bookName, year) = bookName ++ " (" ++ author ++ ", " ++ show(year) ++ ")"


--Number 5
bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec [] = "No books in the list!"
bibliography_rec [(author, bookName, year)] = citeBook(author, bookName, year)
bibliography_rec (x:xs) = citeBook(x) ++ "\n" ++ bibliography_rec(xs)
--
--bibliography_rec (x:xs) = putStr(citeBook(x) ++ "\n ")
--bibliography_rec (x:xs) = putStr(bibliography_rec(xs) ++ "\n ")
--

--Number 6
averageYear :: [(String, String, Int)] -> Int
averageYear [] = 0
averageYear [(str1, str2, year)] = year
averageYear xs = sum listOfNumbers `div` length (listOfNumbers)
    where listOfNumbers = map getInt xs
-- getInt is to get Int values of tuple.
getInt :: (String, String, Int) -> Int
getInt (a, b, c) = c

--Number 7
txt :: String
txt = "[1] and [2] both feature characters who will do whatever it takes to " ++
      "get to their goal, and in the end the thing they want the most ends " ++
      "up destroying them.  In case of [2] this is a whale..."

-- This is to filter using bool 
isReference :: String -> Bool
isReference xs = case xs of ('[':_) -> True  --We want to look for []
                            (x:_) -> False --We do not want words without []
                   
                            
references :: String -> Int
references inputString = length(listOfRefs)
    where listOfRefs = filter isReference listOfWords
          listOfWords = words inputString

--Number 8
citeText :: [(String, String, Int)] -> String -> String
let gatsby = ("F. Scott Fitzgerald", "Great Gatsby", 1925)
let moby = ("Herman Melville", "Moby Dick", 1851)

citeText str =

main = do
	putStrLn (bibliography_rec[("Adel", "book", 12), ("Me", "book1", 13)])