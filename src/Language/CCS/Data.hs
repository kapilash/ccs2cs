{-
Data types exported by CCS
-}
module Language.CCS.Data where
import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

newtype CLib = CLib String
               deriving (Ord, Eq, Show)

newtype CInclude = CInclude String
                 deriving (Ord, Eq, Show)

data CompOpt = CompOptDef String
               | CompOptKV String String
               deriving Show


newtype CStruc = CStruct String
                 deriving (Ord, Eq, Show)

data OSName = WINDOWS
               | FREEBSD
               | LINUX
               | OSX
              deriving (Eq, Show)

data NativeLang = CLang
                | CPlusPlus
                | ObjectiveC
                  deriving (Eq, Show)

newtype OSGroup = OSGroup (OSName, NativeLang)
                  deriving (Eq, Show)

newtype CSPrologue = CSPrologue [String]
                     deriving Show

data FieldValue = CField String
                | HField String
                | FieldTxt String
                 deriving (Eq,Show)

newtype CMacro = CMacro String
                 deriving (Eq,Show)

data EnumValue = EmptyEnum
               | FromMacro String
               | EnumText String
               | EnumSize String
               | EnumOffset String String
               | EnumComplex [EnumValue]
                 deriving (Eq,Show)

data CSVal = CFieldOffset String String
           | CHashDef String
           | CSVerbatim String
           | CDblHash
           | CSizeOf String
           deriving (Eq,Show)

data CCSField = CCSField { ccsfName   :: String,
                           ccsfCSType :: String,
                           ccsfVal    :: FieldValue
                         }
              deriving (Eq,Show)


data CCSType = CSEnum  String  [(String, EnumValue)]
               | CSClass String [(String, String, EnumValue)]
               | CSStruct String String [(String, String, String)]
                 deriving (Eq,Show)



data CCSFile = CCSFile {
                 ccsPrologue    :: CSPrologue
                ,ccsLib         :: CLib
                ,ccsIndent      :: Int
                ,ccsIncludes    :: [CInclude]
                ,ccsTypes       ::   [CCSType]
                ,ccsEpilogue  :: [CSVal]
                }
               deriving Show

data NativeTxt = MacroDef String
                 | StructSize String
                 | StructOffset String String
                   deriving (Eq, Show)




isMacro :: NativeTxt -> Bool
isMacro (MacroDef _) = True
isMacro _            = False

isNotMacro :: NativeTxt -> Bool
isNotMacro = not . isMacro

newtype NativeVal =  NativeVal String
                  deriving (Eq, Show)


hwsalt :: Int -> NativeTxt -> Int
hwsalt i (MacroDef s) = hashWithSalt (hashWithSalt i s)  "@$Macro"
hwsalt i (StructSize s) = hashWithSalt (hashWithSalt i s) "@$Sizeof"
hwsalt i (StructOffset s f) = hashWithSalt (hashWithSalt (hashWithSalt i s) f) "@$Sizeof"

instance Hashable NativeTxt where
  hashWithSalt = hwsalt


type CCSMap = HM.HashMap NativeTxt NativeVal
type CCSet  = HS.HashSet NativeTxt

printNTV :: (NativeTxt , NativeVal) -> IO ()
printNTV (n, NativeVal s) = putStrLn $ (show n) ++ " = " ++ s


addFieldVal str (CField f) set = HS.insert (StructOffset str f) set
addFieldVal _      (HField h) set = HS.insert (MacroDef h) set
addFieldVal _      _          set = set

addMacro str set = HS.insert (MacroDef str) set

addSizeOf str set = HS.insert (StructSize str) set

addOffset str f set = HS.insert (StructOffset str f) set

{-
addField :: String -> HS.HashSet NativeTxt -> CCSField -> HS.HashSet NativeTxt
addField str set (CCSField _ _ (CField f)) = HS.insert (StructOffset str f) set
addField _ set (CCSField _ _ (HField h)) = HS.insert (MacroDef h) set
addField _ set  _                       = set

addEnumMem :: HS.HashSet NativeTxt ->  EnumValue -> HS.HashSet NativeTxt
addEnumMem set (FromMacro s) = HS.insert (MacroDef s) set
addEnumMem set (EnumComplex v) = foldl addEnumMem set v
addEnumMem set _               = set


addFrag :: HS.HashSet NativeTxt -> CSVal -> HS.HashSet NativeTxt
addFrag set (CFieldOffset str f) = HS.insert (StructOffset str f) set
addFrag set (CHashDef s)         = HS.insert (MacroDef s) set
addFrag set (CSizeOf s)          = HS.insert (StructSize s) set
addFrag set _                    = set



addCSType :: HS.HashSet NativeTxt -> CCSType -> HS.HashSet NativeTxt
addCSType set (CSRecord _ _ s fields) = foldl (addField s) set fields
addCSType set (CSEnum _ lst)          = foldl addEnumMem set $ map snd lst


ccsFileToNativeSet :: CCSFile -> HS.HashSet NativeTxt
ccsFileToNativeSet ccsFile = foldl addFrag (foldl addCSType HS.empty (ccsTypes ccsFile)) (ccsEpilogue ccsFile)
-}
