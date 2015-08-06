module Language.CCS.Parser where

import Language.CCS.Data
import qualified Data.List as Lst
import Text.Parsec
import qualified Data.List as Lst
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

type Parser = Parsec String (HS.HashSet NativeTxt)

header :: Parser CInclude
header = do
  name <- between (char '<') (char '>') (many1 $ noneOf "<>#?:\"\'\r\n\t")
  spaces
  return $ CInclude name
  
library :: Parser CLib
library = do
  name <- between (char '"') (char '"') (many1 $ noneOf "<>#?:\'\r\n\t\"")
  spaces
  return $ CLib name

compilerOption :: Parser CompOpt
compilerOption = do
  l <- many1 (alphaNum <|> char '-')
  spaces
  maybev <- option Nothing optval
  spaces
  case maybev of
   Nothing -> return $ CompOptDef l
   Just v  -> return $ CompOptKV  l v

optval  :: Parser (Maybe String)
optval = do
  char '='
  spaces
  l <- manyTill anyChar  endOfLine
  return (Just l)

symbol :: String -> Parser String
symbol str = do
  s <- string str
  spaces
  return s

keyword :: String -> Parser ()
keyword str = do
  string str
  notFollowedBy alphaNum
  spaces
  

osWin32 = do
  keyword "Win32"
  return WINDOWS

osX = do
  keyword "OSX"
  return OSX

osLinux = do
  keyword "Linux"
  return LINUX


osBSD = do
  keyword "FreeBSD"
  return FREEBSD

language = try cpp <|> objc <|> clang
           where cpp = do {keyword "c++"; return CPlusPlus}
                 objc = do {keyword "objc"; return ObjectiveC}
                 clang = do {keyword "c" ; return CLang}

os = try osWin32
     <|> try osX
     <|> try osLinux
     <|> osBSD

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
  spaces
  where isEqualTo' = char '=' <|> (char ':')

comma = symbol ","

endPrologue = do
  keyword "definitions"
  keyword "from"
  openSqBracket
  return $ Just ()

prologue :: Parser CSPrologue
prologue = do
  lines <- lineOrOsgs []
  return (CSPrologue lines)
    where lineOrOsgs x = do
            optOsg <- option Nothing (try endPrologue)
            case optOsg of
             Nothing -> do { p <- manyTill anyChar endOfLine; lineOrOsgs (p:x)}
             Just _  -> return (reverse x)
  


headers :: Parser [CInclude]
headers = do
  keyword "headers"
  isEqualTo
  sepBy1 header comma

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
  spaces
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
  string "epilogue"
  spaces
  isEqualTo
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
   spaces
   return (i:rest)

ccsFile = do
  prol <- prologue
  l <- libSection
  extra <- option [] extraLibs
  hdrs <- headers
  closeSqBracket
  types <- between openBrace (char '}') (many cstype)
  epi <- epilogue
  set <- getState
  return $ (set, CCSFile prol l extra hdrs types epi)
  
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
  spaces
  string "="
  t <- manyTill  anyChar (try $ outMacroEnd)
  modifyState $ HM.insert (MacroDef i) (NativeVal t)


coutSizeOf :: OutParser ()
coutSizeOf = do
  string "sizeof"
  spaces
  i <- csIdent
  spaces
  string "=("
  t <- manyTill  anyChar (try $ cOutEndLine)
  modifyState $ HM.insert (StructSize i) (NativeVal t)


coutOffset :: OutParser ()
coutOffset = do
  string "offset"
  spaces
  s <- csIdent
  spaces
  f <- csIdent
  spaces
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
