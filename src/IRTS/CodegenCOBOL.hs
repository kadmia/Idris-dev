module IRTS.CodegenCOBOL where

import Idris.AbsSyntax
import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.System
import IRTS.CodegenCommon
import Idris.Core.TT
import Paths_idris
import Util.System

import Debug.Trace
import Data.List

data COBOLProgram = COBOLProgram {
                      program_name :: String
                    , procedure_division :: [COBOLStmt]                    
                    }

data COBOLStmt = Display String | Stop

codegenCOBOL :: [(Name, SDecl)] ->
                String -> -- output file name
                OutputType ->   -- generate executable if True, only .o if False
                IO ()
codegenCOBOL defs out exec = do mapM_ putStrLn $ map (\(n,d) -> show n ++ "\t=\t" ++ show d) defs
                                mapM_ putStrLn $ map (generateProgram . compileFun . snd) defs

generateProgram :: COBOLProgram -> String
generateProgram prog = atCol 8 "IDENTIFICATION DIVISION.\n" ++
                       atCol 8 "PROGRAM-ID. " ++ program_name prog ++ ".\n" ++
                       atCol 8 "PROCEDURE DIVISION.\n" ++
                       atCol 8 (concatMap ((++ "\n") .  generateStmt) (procedure_division prog))

generateStmt :: COBOLStmt -> String
generateStmt Stop = "STOP."
generateStmt (Display str) = "DISPLAY '" ++ str ++ "'."

compileFun :: SDecl -> COBOLProgram
compileFun (SFun f args i body) = COBOLProgram (show f) (compileExp body)

compileExp :: SExp -> [COBOLStmt]
compileExp (SError msg) = [Display msg, Stop] 
--compileExp (SLet lvar rhs body) = 
compileExp e = trace ("Couldn't compile SExp " ++ show e) []


replaceSeq :: Eq a => [a] -> [a] -> [a] -> [a]
replaceSeq from to input@(hd:tl) = if from `isPrefixOf` input
                                      then to ++ replaceSeq from to (drop (length from) input)
                                      else hd : replaceSeq from to tl
replaceSeq from to [] = []

atCol :: Int -> String -> String
atCol n str = indent ++ indented str
  where indented = replaceSeq "\n" ('\n':indent)
        indent = replicate n ' '
