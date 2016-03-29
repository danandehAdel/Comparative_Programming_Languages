
import Data.List

data BST k v = Empty |
               Node k v (BST k v) (BST k v)

-- Question 1
val :: BST k v -> Maybe v
val Empty = Nothing
val (Node _ v _ _ ) = Just v

-- Question 2
size :: BST k v -> Int
size Empty = 0
size (Node _ _ l r) = size l + size r + 1

-- Question 3
ins :: (Ord k) => k -> v -> BST k v -> BST k v
ins k v Empty = Node k v Empty Empty
ins k' v' (Node k v l r) 
    | k == k' = Node k' v' l r
    | k' < k = Node k v (ins k' v' l) r
    | k' > k = Node k v l (ins k' v' r)

-- Question 4
instance (Show v) => Show (BST k v) where
  show Empty = ""
  show (Node k v left right) = "(" ++ (show left) ++ (show v) ++ (show right) ++ ")"

-- Question 5
data JSON = JStr String
  | JNum Double
  | JArr [JSON]
  | JObj [(String, JSON)]

instance Show JSON where
  show j = showJson j

showJson :: JSON -> String
showJson (JStr j) = (show j)
showJson (JNum n) = (show n)
showJson (JArr []) = "[]" --base case for JArr
showJson (JArr (x:xs)) = intercalate " " [show(x:xs)]
showJson (JObj []) = "{}" --base case for JObj
showJson (JObj x) = "{" ++ init (showObj (JObj x)) ++ "}"

-- Helper function to manipulate list in JObj - recursively puts the tuple elements together in the specified format 
showObj :: JSON -> String
showObj (JObj []) = ""
showObj (JObj ((a,b):xs)) = (show a) ++ ":" ++(show b) ++ "," ++ showObj (JObj xs)

-- Question 6 
-- Importantly, a static type cast is required when running, that is "2.0 `elem` (fromJson (JArr [JNum 2]) :: [Double])" NOT the test case from ecommons. 
class Json a where
  toJson :: a -> JSON
  fromJson :: JSON -> a

instance Json Double where
  toJson a = (JNum a)
  fromJson (JNum a) =  a

instance (Json a) => Json [a] where 
  toJson xs = JArr (map toJson xs)
  fromJson (JArr []) = [] --base case for JArr
  fromJson (JArr xs) = map fromJson xs