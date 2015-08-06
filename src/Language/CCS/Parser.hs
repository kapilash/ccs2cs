module Language.CCS.Parser where

import Language.CCS.Data
import qualified Data.List as Lst
import Text.Parsec
import qualified Data.List as Lst
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

type Parser = Parsec String (HS.HashSet NativeTxt)




ignorable :: (Monad m) => ParsecT String u m ()
ignorable =  do
  many (space <|> comment)
  return ()
    where
      comment = do {string "/*"; _ <- manyTill anyChar (try $ string "*/"); return ' '}




preprocLine :: Parsec String u CInclude
preprocLine = do
  char '#'
  v <- validDirective
  line <- manyTill anyChar (try endOfLine)
  ignorable
  return . CInclude $ '#':(v ++ line)
    where validDirective = try ( string "include")
                           <|> try (string "if")
                           <|> try (string "endif")
                           <|> try (string "else")
                           <|> try (string "define")
                           <|> try (string "pragma")
                           <|> try (string "undef")

ptest :: (Show a) =>  Parsec String u a -> u -> String -> IO ()
ptest p u txt = do
  putStrLn txt
  case runParser p u "ptest" txt of
   Left err      -> print err
   Right a       -> putStrLn (show a)


beginLine :: Parser (Maybe (CLib, Int))
beginLine = do
  char '#'
  keyword "BEGIN"
  l <- library
  inden <- option 4 indentation
  ignorable
  return $ Just (l, inden)
    where indentation = do i <- between (char '(') (char ')') digit
                           return $ (fromEnum i) -  (fromEnum '0')

header :: Parser CInclude
header = do
  name <- between (char '<') (char '>') (many1 $ noneOf "<>#?:\"\'\r\n\t")
  ignorable
  return $ CInclude name
  
library :: Parser CLib
library = do
  name <- between (char '"') (char '"') (many1 $ noneOf "<>#?:\'\r\n\t\"")
  ignorable
  return $ CLib name

compilerOption :: Parser CompOpt
compilerOption = do
  l <- many1 (alphaNum <|> char '-')
  ignorable
  maybev <- option Nothing optval
  ignorable
  case maybev of
   Nothing -> return $ CompOptDef l
   Just v  -> return $ CompOptKV  l v

optval  :: Parser (Maybe String)
optval = do
  char '='
  ignorable
  l <- manyTill anyChar  endOfLine
  return (Just l)

symbol :: String -> Parser String
symbol str = do
  s <- string str
  ignorable
  return s

keyword :: String -> Parser ()
keyword str = do
  string str
  notFollowedBy alphaNum
  ignorable
  


openSqBracket :: Parser ()
openSqBracket = do
  symbol "["
  return ()

closeSqBracket :: Parser ()
closeSqBracket = do
  symbol "]"
  return ()


isEqualTo = do
  isEqualTo'
  ignorable
  where isEqualTo' = char '=' <|> (char ':')

comma = symbol ","

prologue :: Parser (CSPrologue, CLib, Int)
prologue =   lineOrOsgs []
    where lineOrOsgs x = do
            optOsg <- option Nothing (try beginLine)
            case optOsg of
             Nothing -> do { p <- manyTill anyChar endOfLine; lineOrOsgs (p:x)}
             Just (l,i)  -> return (CSPrologue $reverse x,l,i)

headers :: Parser [CInclude]
headers = many1 preprocLine

libSection :: Parser CLib
libSection = do
  keyword "library"
  isEqualTo
  library

extraLibs :: Parser [CLib]
extraLibs = do
  keyword "dependencies"
  isEqualTo
  openBrace
  ls <- sepBy library comma
  closeBrace
  return ls

openBrace =   symbol "{"
closeBrace = symbol "}"

semicolon = symbol ";"

openBracket = symbol "("
closeBracket = symbol ")"

options :: Parser [CompOpt]
options = do
  keyword "options"
  isEqualTo
  between openBrace closeBrace (many compilerOption)


csIdent :: (Monad m) => ParsecT String u m String
csIdent = do
  f <- letter
  rst <- many (alphaNum <|> char '_')
  ignorable
  return (f:rst)



enumDef = do
  keyword "enum"
  csid <- csIdent
  mems <- between openBrace closeBrace (sepEndBy enumMember comma)
  return $ CSEnum  csid mems
  where
    enumMember = do
      name <- csIdent
      v <- option EmptyEnum (symbol "=" >> enumValue)
      return (name, v)

classDef = do
  keyword "class"
  csid <- csIdent
  mems <- between openBrace closeBrace (many clsMember)
  return $ CSClass csid mems
    where
      clsMember = do
        tn <- typeName
        i <- csIdent
        isEqualTo
        v <- enumValue
        semicolon
        return (tn,i,v)

structDef = do
  keyword "struct"
  csid <- csIdent
  isEqualTo
  cName <- csIdent
  mems <- between openBrace closeBrace (many strMember)
  return $ CSStruct csid cName mems
 where
   strMember = do
     tn <- typeName
     csField <- csIdent
     isEqualTo
     cfield <- csIdent
     semicolon
     return (tn, csField, cfield)

cstype = enumDef <|> classDef <|> structDef

hashedEnumVal :: Parser EnumValue
hashedEnumVal = do
     char '#'
     t <- (enumMacro <|> try enumSize <|> enumOffset)
     return t
 where
   enumMacro :: Parser EnumValue
   enumMacro = do
     ident <- csIdent
     updateState (addMacro ident)
     return $ FromMacro ident

   enumSize :: Parser EnumValue
   enumSize = do
     keyword "$sizeof"
     str <- between openBracket closeBracket csIdent
     updateState (addSizeOf str)
     return $ EnumSize str

   enumOffset :: Parser EnumValue
   enumOffset = do
     keyword "$offset"
     openBracket
     str <- csIdent
     comma
     fld <- csIdent
     closeBracket
     updateState (addOffset str fld)
     return $ EnumOffset str fld

enumComplex  :: Parser EnumValue
enumComplex = do
     openBrace
     parts <- many1 (enumValue <|> asIsCode EnumText)
     closeBrace
     return $ EnumComplex parts
  where
    asIsCode :: (String -> a) -> Parser a
    asIsCode f = do
      t <- many1 $ noneOf "$#{}"
      return $ f t

enumValue = hashedEnumVal <|> enumComplex
            <?> "Enum Value"

csVerbatim = do
  code <- many1 $ noneOf "$#"
  return $ CSVerbatim code

epiMarker = do
  keyword "#END"
  manyTill anyChar (try endOfLine)
  return ()
  
epilogue = do
  v <- many (csVerbatim <|> hashFragment)
  eof
  return v


hashFragment :: Parser CSVal
hashFragment = do
  char '#'
  (cshd <|> try csfalign <|> csdblhash <|> csizeof)
  where 
    csdblhash = do
      char '#'
      return $ CDblHash
    cshd = do
      s <- csIdent
      return $ CHashDef s
  
    csfalign = do
      string "$offset"
      openBracket
      s <- csIdent
      f <- csIdent
      char ')'
      updateState (addOffset s f)
      return $ CFieldOffset s f

    csizeof = do
      string "$sizeof"
      openBracket
      s <- csIdent
      char ')'
      updateState (addSizeOf s)
      return $ CSizeOf s

  

typeName :: Parser String
typeName = do
   i <- letter
   rest <- many (alphaNum <|> char '_' <|> char '.')
   ignorable
   return (i:rest)

ccsFile = do
  (prol,l,i) <- prologue
  hdrs <- headers
  types <- manyTill cstype epiMarker
  epi <- epilogue
  set <- getState
  return $ (set, CCSFile prol l i hdrs types epi)
  
type OutParser = Parsec String  (HM.HashMap NativeTxt NativeVal)


outMacroEnd :: OutParser ()
outMacroEnd = do
  string "-;;-"
  endOfLine
  return ()

cOutEndLine :: OutParser ()
cOutEndLine = do
  string ");"
  endOfLine
  return ()
  
mOutMacro :: OutParser ()
mOutMacro = do
  string "macro"
  char '_'
  i <- csIdent
  ignorable
  string "="
  t <- manyTill  anyChar (try $ outMacroEnd)
  modifyState $ HM.insert (MacroDef i) (NativeVal t)


coutSizeOf :: OutParser ()
coutSizeOf = do
  string "sizeof"
  ignorable
  i <- csIdent
  ignorable
  string "=("
  t <- manyTill  anyChar (try $ cOutEndLine)
  modifyState $ HM.insert (StructSize i) (NativeVal t)


coutOffset :: OutParser ()
coutOffset = do
  string "offset"
  ignorable
  s <- csIdent
  ignorable
  f <- csIdent
  ignorable
  string "=("
  t <- manyTill  anyChar (try $ cOutEndLine)
  modifyState $ HM.insert (StructOffset s f) (NativeVal t)

cOutParser :: OutParser (HM.HashMap NativeTxt NativeVal)
cOutParser = do
  many1 cout
  eof
  getState
    where cout = coutSizeOf <|> coutOffset

macroStart = do
  string "--CCS2CS--Macro-Expansions--"
  endOfLine
  return ()

mOutParser :: OutParser (HM.HashMap NativeTxt NativeVal)
mOutParser = do
  manyTill anyChar (try macroStart)
  many1 mOutMacro
  getState
