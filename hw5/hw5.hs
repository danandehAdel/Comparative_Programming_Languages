
import Data.Char
import Control.Monad
import Control.Applicative (Applicative,pure,(<*>))

-- Parser Framework

data Parser a = Parser (String -> [(a,String)])

run :: Parser a -> String -> [(a,String)]
run (Parser f) s = f s

satP :: (Char -> Bool) -> Parser Char
satP pred = Parser (\cs -> case cs of
                            []    -> []
                            c:cs' -> if pred c then [(c,cs')] else [])

digit = satP isDigit

instance Monad Parser where
  return a = Parser (\cs -> [(a,cs)])
  pa >>= fpb = Parser (\cs -> do (a,cs') <- run pa cs
                                 run (fpb a) cs')

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser (\cs -> run p2 cs ++ run p1 cs)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = return [] <|> oneOrMore p

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do x <- p
                 xs <- zeroOrMore p
                 return (x:xs)

first :: Parser a -> Parser a
first p = Parser (\cs -> case run p cs of
                          [] -> []
                          (r:rs) -> [r])

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p opp = do x <- p
                 tryMore x
  where tryMore x = first (return x <|> more x)
        more x = do op <- opp
                    y <- p
                    tryMore (op x y)

-- Question 4
dotDouble = satP (=='.') -- check for dot 

doubleP :: Parser Double
doubleP = first (intCheck <|> doubleCheck)

-- Check for the structure/format of a double, done first (please note double contains a decimal point)
doubleCheck = do digits <- first (oneOrMore digit)
                 dot <- dotDouble
                 remDigits <- first (zeroOrMore digit)
                 return (read (digits ++ (dot:remDigits)))

-- Otherwise check for the structure of an int
intCheck = do digits <- first (oneOrMore digit)
              return (read digits)

addOp :: Parser (Double -> Double -> Double) 
addOp = do satP (== '+') ; return (+)
    <|> do satP (== '-') ; return (-)   

mulOp :: Parser (Double -> Double -> Double) 
mulOp = do satP (== '*') ; return (*)
    <|> do satP (== '/') ; return (/) 

calc :: Parser Double
calc = let mulExpr = chain doubleP mulOp 
       in  chain mulExpr addOp 

-- Question 5
expOp :: Parser (Double -> Double -> Double)
expOp = do satP (== '^') ; return (**) -- ** is exponent operator for doubles/floating 

calc2 :: Parser Double
calc2 = let expExpr = chain doubleP expOp
            mulExpr = chain expExpr mulOp
        in  chain mulExpr addOp

-- Question 6
parensP :: Parser Double
parensP = doubleP <|> parens  -- check for appropriate structure for parenthesis and call calc for what is inside. 
  where parens = do satP (== '(')
                    calcInside <- calc3
                    satP (== ')')
                    return calcInside

calc3 :: Parser Double
calc3 = let expExpr = chain parensP expOp
            mulExpr = chain expExpr mulOp
        in  chain mulExpr addOp