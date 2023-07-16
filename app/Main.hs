import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (findIndices)
import System.IO

type Memory a = [(String, a)]
type Env = Memory Int

update :: Memory a -> String -> a -> Memory a
update [] key value = [(key, value)]
update ((k,v):mem) key value
  | k == key = (key, value) : mem
  | otherwise = (k,v) : update mem key value

find :: Memory a -> String -> Maybe a
find [] _ = Nothing
find ((k,v):mem) key
  | k == key = Just v
  | otherwise = find mem key

data Expr = Var String
          | Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show)

type MParser a = Parsec String (Memory Int) a

eval :: Expr -> Env -> Int
eval (Num n) _ = n
eval (Add e1 e2) env = eval e1 env + eval e2 env
eval (Sub e1 e2) env = eval e1 env - eval e2 env
eval (Mul e1 e2) env = eval e1 env * eval e2 env
eval (Div e1 e2) env = eval e1 env `div` eval e2 env
eval (Var var) env = case find env var of
  Just v -> v
  Nothing -> error "Variable not found"

defn :: MParser Expr
defn = do
  (Var varname) <- variable
  char '='
  env <- getState
  val <- flip eval env <$> expr
  char ';'
  many $ char ' '
  char '\n'
  modifyState (\m -> update m varname val)
  return $ Var varname

expr :: MParser Expr
expr = term `chainl1` addop

addop :: MParser (Expr -> Expr -> Expr)
addop = (char '+' *> return Add) <|> (char '-' *> return Sub)

term :: MParser Expr
term = factor `chainl1` mulop

mulop :: MParser (Expr -> Expr -> Expr)
mulop = (char '*' *> return Mul) <|> (char '/' *> return Div)

factor :: MParser Expr
factor = between (char '(') (char ')') expr <|> number <|> variable

number :: MParser Expr
number = Num . read <$> many1 digit <|> do
  char '-'
  Num . negate . read <$> many1 digit

variable :: MParser Expr
variable = Var <$> (char '$' *> many1 letter)

program :: MParser Int
program = do
  many defn
  env <- getState
  e <- expr
  many $ char ' '
  eof
  return $ eval e env

-- Example usage
main :: IO ()
main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  case runParser program [] "" contents of
    Left err -> print err
    Right res -> print res
  hClose handle
