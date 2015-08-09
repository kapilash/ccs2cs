module Language.CCS.Printer where

import Language.CCS.Data
import Text.PrettyPrint 
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM


includeToC :: CInclude -> Doc
includeToC (CInclude s) = text s

headersToC = vcat . map includeToC 


nativeTxtToM (StructOffset _ _) = empty
nativeTxtToM (StructSize _) = empty
nativeTxtToM (MacroDef _ s) = text "macro_" <> (text s)
                            <+> text "="
                            <> (text s)
                            <> text "-;;-"


ctypeToDoc CInt = (text "int", text "=(%lu)")
ctypeToDoc CChar = (text "char", text "=(%c)")
ctypeToDoc CStr  = (text "str", text "=(%s)")
ctypeToDoc CFloat = (text "float", text "=(%lf)")
ctypeToDoc _ = (text "long", text "=(%lu)")


macroPrintStmt :: String -> (Doc,Doc) -> Doc
macroPrintStmt s (d,f) = nest 8 $ text "printf("
                         <> (doubleQuotes $ d <+> text s <+> f <> text ";\\n")
                         <+> text ","
                         <+> text s
                         <+> text ");"

structSizePrintStmt :: String -> Doc
structSizePrintStmt s = nest 8 $ text "printf("
                        <> (doubleQuotes $ text "sizeof" <+> text s <+> text "=(%lu);\\n")
                        <+> text ","
                        <+> text "sizeof(struct" <+> text s <> text ")"
                        <+> text ");"

offsetPrintStmt :: String -> String -> Doc
offsetPrintStmt s f = nest 8 $ text "printf("
                      <> (doubleQuotes $ text "offset" <+> text s <+> text f <+> text "=(%lu);\\n")
                        <+> text ","
                        <+> text "offsetof(struct" <+> text s <> text "," <+> text f <> text ")"
                        <+> text ");"

nativeTxtToC (MacroDef t s) = macroPrintStmt s (ctypeToDoc t)
nativeTxtToC (StructSize s) = structSizePrintStmt s
nativeTxtToC (StructOffset s f) = offsetPrintStmt s f


printStmts =  vcat . map nativeTxtToC . filter isNotMacro .  HS.toList 

macroStmts = vcat . map nativeTxtToM . filter isMacro . HS.toList

mainBegin :: Doc
mainBegin = text "int main(int argc, char **argv){"

mainClose :: Doc
mainClose = vcat [ nest 8 $ text "return 0;", text "}"]

mCodeBegin :: Doc
mCodeBegin = text "--CCS2CS--Macro-Expansions--"


stdhdrs = map CInclude ["#include <stdio.h>", "#include <stddef.h>","#include <string.h>"]

appendHdrs :: [CInclude] -> [CInclude]
appendHdrs hdrs = stdhdrs ++ hdrs


cCode :: HS.HashSet NativeTxt -> [CInclude] -> Doc
cCode set hdrs = vcat [headersToC . appendHdrs $ hdrs, mainBegin, printStmts set, mainClose]

mCode :: HS.HashSet NativeTxt -> [CInclude] -> Doc
mCode set hdrs = vcat [headersToC . appendHdrs $ hdrs, mCodeBegin, macroStmts set]


prologToDoc :: CSPrologue -> Doc
prologToDoc (CSPrologue lines) = vcat $ map text lines



nativeToDoc (NativeVal x) = text x

unkval = NativeVal "**UNKNOWN**"

extractNMap :: NativeTxt -> CCSMap ->  Doc
extractNMap n = nativeToDoc . HM.lookupDefault unkval n

csValToDoc :: CCSMap -> CSVal -> Doc
csValToDoc nm (CFieldOffset s v) = extractNMap (StructOffset s v) nm
csValToDoc nm (CHashDef t s)       = extractNMap (MacroDef t s) nm
csValToDoc nm (CSizeOf s)        = extractNMap (StructSize s) nm
csValToDoc _ (CSVerbatim txt)   = text txt
csValToDoc _ CDblHash           = text "#"

epilogToDoc nm vals = hcat $ map (csValToDoc nm) vals


enumValToDoc :: CCSMap -> EnumValue -> Doc
enumValToDoc nm EmptyEnum = empty
enumValToDoc nm (FromMacro s) = extractNMap (MacroDef CInt s) nm
enumValToDoc nm (EnumText s) = text s
enumValToDoc nm (EnumComplex vs) = hcat $ map (enumValToDoc nm) vs
enumValToDoc nm (EnumSize s)     = extractNMap (StructSize s) nm
enumValToDoc nm (EnumOffset s f) = extractNMap (StructOffset s f) nm


enumFieldToDoc :: CCSMap -> (String, EnumValue)  -> Doc
enumFieldToDoc nm (name, EmptyEnum) = text name
enumFieldToDoc nm (name, v) = nest 4 $ text name <+> text "=" <+> (enumValToDoc nm v)

enumFieldsToDoc :: CCSMap -> [(String, EnumValue)] -> Doc
enumFieldsToDoc _ [] = empty
enumFieldsToDoc nm [single] =  enumFieldToDoc nm single
enumFieldsToDoc nm (f:s:rest) =  enumFieldToDoc nm f <+> comma $+$ (enumFieldsToDoc nm (s:rest))

clsFieldToDoc :: CCSMap -> (String ,String, EnumValue) -> Doc
clsFieldToDoc _ (typ,f,  EmptyEnum) = nest 4 $ text "internal" <+> text "static" <+> text typ <+> text  "{ get; set; }"
clsFieldToDoc nm (typ, f, EnumText s)  = nest 4 $ hsep [text "internal", text "static", text typ , text f , text "=" , text s, semi]
clsFieldToDoc nm (typ, f, e )  = nest 4 $ hsep [text "internal", text "const", text typ , text f , text "=" , enumValToDoc nm e, semi]
                                
enumToDoc :: CCSMap -> String -> [(String, EnumValue)] -> Doc
enumToDoc nm  enumName lst =  enumDef
  where enumDef = vcat $ [text "internal" <+> text "enum" <+> text enumName,
                          lbrace,
                          enumFieldsToDoc nm lst,
                          rbrace]

clsToDoc :: CCSMap ->String -> [(String, String, EnumValue)] -> Doc
clsToDoc nm enumName lst =  clsDef
  where clsDef = (vcat $ (text "internal" <+> text "static" <+> text "class" <+> text enumName <> lbrace) : (map (clsFieldToDoc nm) lst) ) $$ rbrace


strToDoc :: CCSMap -> String ->String -> [(String, String, String)] -> Doc
strToDoc nm clsName cstr lst =  strDef
  where strDef = vcat $ [text "internal" <+>  text "class" <+> text clsName <> lbrace , consdef] ++  (map strFieldToDoc lst) ++ [genMeth,rbrace]
        consdef =  nest 4 $ hsep [ text "private" , text clsName , text "()", lbrace, rbrace]
        genMeth =  nest 4 $ vcat $ [ text "public"  <+> text "static" <+> text clsName <+> text "Unmarshal(IntPtr _nativePointer)" <+> lbrace, instLine] ++
                   (map marshalFields lst) ++ [nest 4 $ text "return _instance;", rbrace]
        strFieldToDoc (t, f, _) = nest 4 $ hsep [text "internal", text t, text f, semi]
        marshalFields (t,f, cf) = nest 4 $ text "_instance." <> text f <+> equals <+> text "System.Runtime.InteropServices.Marshal.Read" <> text t <>
                                  lparen <> text "_nativePointer" <> comma <+> (extractNMap (StructOffset cstr cf) nm) <> rparen <> semi
        instLine                = nest 4 $ text clsName <+> text "_instance" <+> equals <+> text "new" <+> text clsName <> lparen <> rparen <> semi
                                    


ccsTypeToDoc :: Int -> CCSMap -> CCSType -> Doc
ccsTypeToDoc i nm (CSEnum s lst) = nest i $ enumToDoc nm s lst
ccsTypeToDoc i nm (CSClass s lst) = nest i $ clsToDoc nm s lst
ccsTypeToDoc i nm (CSStruct s1 s2 lst) = nest i $ strToDoc nm s1 s2 lst



ccsFileToDoc :: CCSMap -> CCSFile -> Doc
ccsFileToDoc nm (CCSFile p _ i _ types e) = vcat $ [prologToDoc p ] ++ map (ccsTypeToDoc i nm) types ++ [epilogToDoc nm e]


csFile :: CCSMap -> CCSFile -> String
csFile nmap ccs = render $ ccsFileToDoc nmap ccs
