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
           | CHashDef  String
           | CSVerbatim String
           | CDblHash
           | CSizeOf String
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


data  NativeTxt = MacroDef String
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
hwsalt i (MacroDef s) = hashWithSalt (hashWithSalt i s)  "@$MacroDef"
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

