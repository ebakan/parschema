module Parschema where 
{-# OPTIONS -Wall #-} 

import System.Environment
import Control.Applicative
import Data.Maybe
import Text.ParserCombinators.Parsec hiding ((<|>), optional, many)
--import Text.Parsec.Prim (ParsecT, Stream)
--import Debug.Trace

data Field = Field { fieldType :: String, fieldName :: String} deriving (Show)
data Table = Table { tableName :: String, tableFields :: [Field]} deriving (Show)
data Schema = Schema [Table] deriving (Show)
data Do = Do {doExpr :: String, doVar :: Maybe String} deriving (Show)
--data Comment = Comment {commentVal :: String} deriving (Show)
--data Def = Def {defName :: String, defArgs :: Maybe [String]} deriving (Show)
--data If = If {ifExpr :: String} deriving (Show)
--data Unless = Unless {unlessExpr :: String} deriving (Show)
--data Case = Case {caseExpr :: String} deriving (Show)
--data When = When {whenExpr :: String} deriving (Show)

--num :: Parser String
--num = many1 digit

word :: Parser String
word = (:) <$> letter <*> many (alphaNum <|> (oneOf "_-."))

pipe :: Parser Char
pipe = char '|'

eol :: Parser ()
eol = nil <$> oneOf "\n\r"

quote :: Parser Char
quote = char '"'

end :: Parser String
end = whitespace *> string "end" <* skipMany (noneOf "\n\r")

whitespace :: Parser (Maybe ())
whitespace = optional spaces

--endline :: Parser ()
--endline = (nil <$> (char ';')) <|> (nil <$> comment) <|> eol <|> eof

--comment :: Parser Comment
--comment = Comment <$> (whitespace *> char '#' *> manyTill anyChar (eol <|> eof))

nil :: a -> ()
nil _ = ()

--maybeTry :: GenParser tok u a -> ParsecT [tok] u Data.Functor.Identity.Identity (Maybe a)
--maybeTry a = optionMaybe $ try a

doHeader :: Parser Do
doHeader = Do <$> manyTill anyChar ( try ( nil <$> whitespace *> string "do" <* whitespace) ) <*> optionMaybe (whitespace *> pipe *> whitespace *> word <* whitespace <* pipe <* whitespace)

--defHeader :: Parser Def
--defHeader = Def <$> (whitespace *> string "def" *> spaces *> word <* whitespace) <*> optionMaybe (char '(' *> whitespace *> (sepBy (trace "teh manies?" (many (noneOf ",)"))) (char ',')) <* whitespace <* char ')')

--basicHeader :: String -> Parser String
--basicHeader s = whitespace *> string s *> spaces *> (manyTill anyChar endline)

--ifHeader :: Parser If
--ifHeader = If <$> basicHeader "if"

--unlessHeader :: Parser Unless
--unlessHeader = Unless <$> basicHeader "unless"

--caseHeader :: Parser Case
--caseHeader = Case <$> basicHeader "case"

--whenHeader :: Parser When
--whenHeader = When <$> basicHeader "when"

stringOrSym :: Parser String
stringOrSym = try (char ':' *> word) <|> (quote *> word <* quote)

--commaParam :: Parser (String, String)
--commaParam = trace "looking for a commaparam" $ (tup <$> (trace "looking for the paramname" (char ',' *> whitespace *> word <* whitespace <* string "=>")) <*> (trace "looking for the paramval" (whitespace *> word)))

field :: String -> Parser Field
field "" = unexpected "No param"
--field e = trace ("fielding " ++ e) Field <$> (trace "looking for the type" (whitespace *> string e *> char '.' *> word)) <*> (trace "looking for the value" (whitespace *> stringOrSym <* (many commaParam)))
field e = do
  f <- Field <$> (whitespace *> string e *> char '.' *> word) <*> (whitespace *> stringOrSym <* skipMany (noneOf "\n\r"))
  return f

tableHeader :: Parser (String, String)
tableHeader = do
  (Do name var) <- doHeader
  case var of
    Just v -> case parse (whitespace *> string "create_table" *> whitespace *> stringOrSym) "" name of 
      Left err -> unexpected $ show err
      Right tablen -> return (tablen, v)
    Nothing -> unexpected "No local variable"

schemaHeader :: Parser ()
schemaHeader = do
  (Do name _) <- doHeader
  case parse (whitespace *> string "ActiveRecord" *> whitespace *> string "::" *> whitespace *> string "Schema.define") "" name of
    Left err -> unexpected $ show err
    Right _ -> return ()

line :: Parser String
line = manyTill (noneOf "\n\r") (eol)

getJust :: [Maybe a] -> [a]
getJust = (map fromJust) . (filter isJust)

table :: Parser Table
table = do
  (name, var) <- tableHeader
  fields <- getJust <$> manyTill ((Just <$> (field var)) <|> ((\x -> Nothing) <$> line)) (try end)
  return $ Table name fields

schema :: Parser Schema
schema = do
  schemaHeader
  tables <- getJust <$> manyTill ((Just <$> (try table)) <|> ((\x -> Nothing) <$> line)) (try end)
  return $ Schema tables

findSchemas :: Parser [Schema]
findSchemas = do
  schemas <- getJust <$> manyTill (Just <$> (try schema) <|> ((\x -> Nothing) <$> line)) (eof)
  return schemas

parschemaFile :: String -> IO (Either ParseError [Schema])
parschemaFile = parseFromFile findSchemas

parschema :: String -> Either ParseError [Schema]
parschema = parse findSchemas ""

showParschemaFile :: String -> IO ()
showParschemaFile s = do
  result <- parschemaFile s
  case result of
    Left err -> do putStr "parse error at "
                   print err
    Right x  -> print x

showParschema :: String -> IO ()
showParschema s = case parschema s of
  Left err -> do putStr "parse error at "
                 print err
  Right x  -> print x
