module Main where

import qualified Data.List as Lst
import Language.CCS.Data
import Language.CCS.Parser
import Language.CCS.Printer
import System.Environment(getArgs)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Text.Parsec
import System.Console.GetOpt
import Text.PrettyPrint
import System.Process
import System.Exit
import System.Info

newtype CPPArgs = CPPArgs [String]
                  deriving Show


compilercmd = if os == "mingw32"
              then "cl.exe"
              else "clang"

preproccmd = if os == "mingw32"
             then "cl.exe"
             else "clang"

asPreProcArgs (CPPArgs strs) f = if os == "mingw32" then  "/E" : (strs ++ [f])
                                 else "-E" : (strs ++ [f])

newtype CompArgs = CompArgs [String]
                   deriving Show




asCompilerArgs (CompArgs strs) f = if os /= "mingw32"
                                   then strs ++ ["-o", "ccsgen.out", f]
                                   else strs ++ ["/o", "ccsgen.exe", f]

executableName = if os == "mingw32" then "ccsgen.exe" else "./ccsgen.out"

data CCSOption  = InclDir String
                | LibDir String
                | DDef String
                | ShowHelp
             deriving (Show,Eq)

ccsOpt2XArg ::CCSOption -> (CompArgs, CPPArgs)  -> (CompArgs, CPPArgs)
ccsOpt2XArg (InclDir s) (CompArgs l1, CPPArgs l2)  = (CompArgs $ ("-I " ++ s) : l1, CPPArgs $ ("-I " ++ s) : l2)
ccsOpt2XArg (LibDir s)  (CompArgs l1, cppArgs)  = (CompArgs $ ("-L " ++ s) : l1, cppArgs)
ccsOpt2XArg (DDef s)    (CompArgs l1, CPPArgs l2) = (CompArgs $ ("-D" ++ s): l1, CPPArgs $ ("-D" ++ s):l2)
ccsOpt2XArg _           x                         = x

toXArgs :: [CCSOption] -> (CompArgs, CPPArgs)
toXArgs = foldr ccsOpt2XArg (CompArgs [], CPPArgs [])

printHelp :: IO ()
printHelp = putStrLn $ usageInfo "ccs2cs.exe\n (c) Microsoft 2015\n" ccsOptions

needsHelp :: [CCSOption] -> Bool
needsHelp = elem ShowHelp

ccsOptions :: [OptDescr CCSOption]
ccsOptions =
  [ Option ['D','d'] ["define","Define"] (ReqArg DDef "define ") "define variables to be passed to compiler and preprocessor"
  , Option ['I','i'] ["include-dir","include"] (ReqArg  InclDir  "DIR") "include directories"
  , Option ['L','l'] ["library-path","libdir"] (ReqArg LibDir "DIR") "directory containing libraries"
  , Option ['h','H']  ["help","Help","HELP"]         (NoArg ShowHelp) "displays help"
  ]
    
readOptions :: [String] -> ([CCSOption],[FilePath])
readOptions args = case getOpt Permute ccsOptions args of
                    (opts,files,[]) -> (opts,files)
                    (_,_, errs) -> ([],[])

genParseTxt :: Parsec String u a -> FilePath -> u ->String -> IO (Maybe a)
genParseTxt parser f state txt =   case runParser parser state f txt of
                                    Right r        -> return $ Just r
                                    Left err       -> do { print err; return Nothing}  

genMCode :: (HS.HashSet NativeTxt, CCSFile) -> FilePath ->IO ()
genMCode (set, ccsfile) f = writeFile f (render $ mCode set (ccsIncludes ccsfile))



runCpp :: FilePath -> CPPArgs -> IO (Maybe (HM.HashMap NativeTxt NativeVal))
runCpp f args = do
  (e,stdout, stderr) <- readProcessWithExitCode preproccmd (asPreProcArgs args f) ""
  case e of
   ExitFailure i  -> do {putStrLn $ "preprocessor failed with exit code " ++ (show i);putStrLn stderr; return Nothing}
   ExitSuccess    -> genParseTxt mOutParser f HM.empty stdout
  

genCCode :: (HS.HashSet NativeTxt, HM.HashMap NativeTxt NativeVal, CCSFile) -> FilePath -> CompArgs -> IO (Maybe (HM.HashMap NativeTxt NativeVal))
genCCode (set,nMap, ccsfile) f args = do
  writeFile f (render $ cCode set (ccsIncludes ccsfile))
  (e, stdout, stderr) <- readProcessWithExitCode compilercmd (asCompilerArgs args f) ""
  case e of
   ExitFailure i -> do {putStrLn $ "compilation failed with exit code " ++ (show i); putStrLn stderr; putStrLn stdout; return Nothing}
   ExitSuccess   -> do
     (e',stdout',_) <- readProcessWithExitCode executableName [] ""
     case e of
      ExitFailure i  -> do {putStrLn "Error in computing values"; return Nothing}
      _              -> genParseTxt cOutParser f nMap stdout'


genCSCode :: (HM.HashMap NativeTxt NativeVal, CCSFile) -> FilePath -> IO ()
genCSCode (m, ccsFile) f =   do
  putStrLn $ "generating " ++ f
  writeFile f (csFile m ccsFile)

parseCCSFile :: FilePath -> IO (Maybe (HS.HashSet NativeTxt, CCSFile))
parseCCSFile f = do
  ccsCode <-  readFile f
  genParseTxt ccsFile f HS.empty ccsCode


printMap :: HM.HashMap NativeTxt NativeVal -> IO ()
printMap nMap = mapM_ printNTV (HM.toList nMap)

actOnFile :: (CompArgs,CPPArgs)-> FilePath -> IO ()
actOnFile (compiler, preproc) f = do
  putStrLn $ "parsing " ++ f
  maybeRes <- parseCCSFile f
  case maybeRes of
   Nothing  -> do {putStrLn "could not parse. Quitting"; return ()}
   Just (set,ccsFile) -> do
     genMCode (set,ccsFile) (f ++ ".m")
     maybePPMap <- runCpp (f ++ ".m") preproc
     case maybePPMap of
      Nothing -> do {putStrLn "failed during preprocessing. Quitting"; return ()}
      Just macrodMap -> do
        maybeFinMap <- genCCode (set, macrodMap, ccsFile) (f ++ ".c") compiler
        case maybeFinMap of
         Nothing  -> do {putStrLn "failed during c compilation.Quitting"; return ()}
         Just fmp -> do {printMap fmp; genCSCode (fmp, ccsFile) (f ++ ".cs")}

main = do
  args <- getArgs
  case readOptions args of
   ([],[])  -> printHelp
   (ccsopts, files) -> mapM_ (actOnFile (toXArgs ccsopts)) files
