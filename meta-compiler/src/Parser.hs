
{-# LANGUAGE PackageImports #-}
module Parser where

--------------------------------------------------------------------------------

import Data.Char
import Data.List
import Data.List.Split (splitOn)

import Control.Monad

import "parsec1" Text.ParserCombinators.Parsec

import Expr
import Macro

--------------------------------------------------------------------------------
-- * parsing expressions

identP :: Parser String
identP = do
  x  <- letter
  xs <- many (alphaNum <|> oneOf "_")
  return (x:xs)

boolP :: Parser Expr
boolP =  try (string "true"  >> return (MkFix $ Kst $ VBool True ))
     <|>     (string "false" >> return (MkFix $ Kst $ VBool False))

decimalNumberP :: Parser Integer
decimalNumberP = do
  ds <- many1 digit
  return (read ds)

hexNumberP :: Parser Integer
hexNumberP = do
  prefix <- string "0x"
  ds <- many1 (digit <|> oneOf "abcdefABCDEF")
  return $ read (prefix ++ ds)

numberP :: Parser Integer
numberP = try hexNumberP <|> decimalNumberP

indexedP :: Parser Expr
indexedP = do
  name <- identP  ; spaces
  indices <- many $ do 
    char '['        ; spaces
    idx <- exprP    ; spaces
    char ']'        ; spaces
    return idx
  return $ foldl (\x y -> MkFix (Idx x y)) (MkFix $ Var name) indices

arrayP :: Parser Expr
arrayP = 
  do
    char '[' ; spaces
    list <- listP
    return $ MkFix $ Arr list
  where

    listP :: Parser [Expr]
    listP = try emptyP <|> try singletonP <|> notSingletonP

    emptyP = do
      char ']'
      spaces
      return []

    singletonP = do
      e <- exprP
      char ']' 
      spaces
      return [e]

    notSingletonP = do
      e <- exprP
      char ',' 
      spaces
      es <- listP
      return (e:es)

exprP :: Parser Expr
exprP 
  =   try logicalOrP
  <|> atomicExprP

atomicExprP :: Parser Expr
atomicExprP 
  =   parenP
  <|> arrayP 
  <|> try boolP
  <|> try indexedP -- identifier 
  <|> number
  where
{-
    identifier = do
      xs <- identP
      spaces
      return $ case xs of
        "true"  -> MkFix $ Kst $ VBool True
        "false" -> MkFix $ Kst $ VBool False
        _       -> MkFix $ Var xs
-}
    number = do
      x <- numberP
      spaces
      return $ MkFix $ Kst $ VInt x

parenP :: Parser Expr
parenP = do
  char '(' ; spaces
  e <- exprP 
  char ')' ; spaces
  return e

stringOneOf :: [String] -> Parser String
stringOneOf []          = pzero
stringOneOf [this]      = string this
stringOneOf (this:rest) = try (string this) <|> stringOneOf rest

logicalOrP :: Parser Expr
logicalOrP =
  do
    e1 <- logicalAndP
    es <- many orP
    return $ foldl f e1 es
  where
    f old (op,new) = case op of
      "||" -> MkFix (Or_ old new)
    orP = do
      op <- string "||"
      spaces
      e <- logicalAndP
      return (op,e)

logicalAndP :: Parser Expr
logicalAndP =
  do
    e1 <- cmpP
    es <- many andP
    return $ foldl f e1 es
  where
    f old (op,new) = case op of
      "&&" -> MkFix (And old new)
    andP = do
      op <- string "&&"
      spaces
      e <- cmpP
      return (op,e)

cmpP :: Parser Expr
cmpP = do
  e1 <- addP
  mb <- optionMaybe $ do
    op <- stringOneOf ["==","<","<=",">",">="]
    spaces
    e2 <- addP
    return (op,e2)
  case mb of
    Nothing      -> return e1
    Just (op,e2) -> return $ case op of
      "==" -> MkFix (Eq_ e1 e2)
      "<=" -> MkFix (Le_ e1 e2)
      "<"  -> MkFix (Lt_ e1 e2)
      ">=" -> MkFix (Le_ e2 e1)
      ">"  -> MkFix (Lt_ e2 e1)
      _    -> error "fatal: unknown comparison operator"

addP :: Parser Expr
addP = 
  do
    e1 <- mulP
    es <- many plusMinusP
    return $ foldl f e1 es
  where
    f old (c,new) = case c of
      '+' -> old + new
      '-' -> old - new
    plusMinusP = do
      c <- oneOf "+-" 
      spaces
      e <- mulP
      return (c,e)

mulP :: Parser Expr
mulP = 
  do
    e1 <- atomicExprP
    es <- many timesDivP
    return $ foldl f e1 es
  where
    f old (c,new) = case c of
      '*' -> old * new
    timesDivP = do
      c <- oneOf "*" 
      spaces
      e <- atomicExprP
      return (c,e)

--------------------------------------------------------------------------------
-- * parsing statements

nakedBlockP :: Parser Block
nakedBlockP = many statementP

blockP :: Parser Block
blockP = do
  char '{' ; spaces
  b <- nakedBlockP
  char '}' ; spaces
  return b

statementP :: Parser Statement
statementP = do
  -- spaces
  metaP <|> try commentOnlyLine <|> midenP

metaP :: Parser Statement
metaP = do
  char '.'
  s <- (forP <|> ifteP <|> assignP)
  spaces
  char ';'
  commentOrEOL
  spaces
  return s

rangeP :: Parser (Expr,Expr)
rangeP = do
  char '['          ; spaces
  exprA <- exprP    ; spaces
  string ".."       ; spaces
  exprB <- exprP    ; spaces
  char ']'          ; spaces
  return (exprA,exprB)

-- .for i = [a..b] { ... }
forP :: Parser Statement
forP = do
  string "for"      ; spaces
  loopvar <- identP ; spaces
  char '='          ; spaces
  (exprA,exprB) <- rangeP
  block <- blockP   
  return $ For loopvar exprA exprB block

-- .if cond { ... } else { ... }
ifteP :: Parser Statement
ifteP = do
  string "if"       ; spaces
  cond <- exprP     ; spaces
  thenB <- blockP   ; spaces
  mbElseB <- optionMaybe $ do
    string "else"     ; spaces
    elseB <- blockP   ; spaces
    return elseB
  return $ IfS cond thenB mbElseB

-- .var x = expr
assignP :: Parser Statement
assignP = do
  string "var"      ; spaces
  name <- identP    ; spaces
  char '='          ; spaces
  rhs <- exprP      ; spaces
  -- commentOrEOL      
  return $ Set name rhs

eolP :: Parser ()
eolP = do
  optional $ char '\r'
  char '\n'
  return ()

commentOnlyLine :: Parser Statement
commentOnlyLine = do
  spaces
  commentOrEOL
  return NOP

commentP :: Parser ()
commentP = do
  oneOf "#"
  many (noneOf "\n\r")
  return ()

commentOrEOL :: Parser ()
commentOrEOL = do
  optional commentP
  eolP 
  spaces

midenLineP :: Parser String
midenLineP = do
  char ':' ; spaces
  stuff <- many1 (noneOf "\n\r#")
  commentOrEOL
  return stuff 

midenP :: Parser Statement
midenP = (Miden . parseMidenLine) <$> midenLineP

parseMidenLine :: String -> Interpolated Name
parseMidenLine = go where

  go str = case splitOn "$" str of
    [x] -> String x
    (x:y:zs) -> case y of
      ('{':ys) -> case span (/='}') ys of
        (ident,rest0) -> case rest0 of
          ('}':rest)    -> Interp x ident (go $ intercalate "$" (rest:zs))
          _             -> error "parseMidenLine: unclosed interpolation variable"
      _ -> case span isIdentChar y of
        (ident,rest) -> Interp x ident (go $ intercalate "$" (rest:zs))

  isIdentChar c = isAlpha c || isDigit c || c=='_'

--------------------------------------------------------------------------------

parse1 :: Parser a -> String -> Either String a
parse1 p what = case runParser p () "<p>" what of
  Left err -> Left (show err)
  Right yy -> Right yy

parseExpr :: String -> Either String Expr
parseExpr what = case runParser exprP () "<expr>" what of
  Left err -> Left (show err)
  Right yy -> Right yy

--------------------------------------------------------------------------------

programP :: Parser Block
programP = do
  spaces 
  prg <- nakedBlockP
  spaces
  eof
  return prg

parseFile :: FilePath -> IO (Either String Block)
parseFile fname = do
  text <- readFile fname
  let text' = (unlines . filter (not . all isSpace) . lines) text ++ "\n"
  -- putStrLn text'
  case runParser programP () fname text of
    Left err -> return $ Left (show err)
    Right yy -> return $ Right yy
