import Data.Char
import Data.Maybe
import Control.Applicative ((<$>), (<*>))

type Operator = Double -> Double -> Double
type Register = [(String, Operator)]

operatorRegister :: Register
operatorRegister = 
  [
    ("-", (-)),
    ("+", (+)),
    ("/", (/)),
    ("*", (*))
  ]

isOperator :: Char -> Bool
isOperator x
  | x == '+' || x == '-' || x == '*' || x == '/' = True
  | otherwise = False
            
calculate :: String -> Maybe Double
calculate expression 
  | broken == Nothing = Nothing
  | otherwise = eval operatorRegister (fromJust broken)
    where
      broken = break' expression
  -- OR calculate expression = eval operatorRegister (fromJust $ break' expression)
            
eval :: Register -> [String] -> Maybe Double
eval [] _ = Nothing
eval _ [] = Nothing
eval _ [number] = Just $ read number
eval ((operator, function) : rest) unparsed =
  case span (/=operator) unparsed of
    (_, []) -> eval rest unparsed
    (beforeOperator, afterOperator) -> 
      function
        <$> (eval operatorRegister beforeOperator)
        <*> (eval operatorRegister $ drop 1 afterOperator)

break' :: String -> Maybe [String]
break' string = breakHelper "" [] string
  where
    breakHelper :: String -> [String] -> String -> Maybe [String]
    breakHelper num container ""
      | num /= "" = Just $ container ++ [num]
      | otherwise = Just $ container
    breakHelper num container (x : xs)
      | x == ' ' && num /= "" = breakHelper "" (container ++ [num]) xs
      | x == ' ' && num == "" = breakHelper "" container xs
      | isOperator x && num /= "" = breakHelper "" (container ++ [num] ++ [[x]]) xs
      | isOperator x && num == "" = breakHelper "" (container ++ [[x]]) xs
      | isNumber x && num /= "" = breakHelper (num ++ [x]) container xs
      | isNumber x && num == "" = breakHelper [x] container xs
      | otherwise = Nothing