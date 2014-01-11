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

codegenCOBOL :: [(Name, SDecl)] ->
                String -> -- output file name
                OutputType ->   -- generate executable if True, only .o if False
                IO ()
codegenCOBOL defs out exec = mapM_ putStrLn $ map (\(n,d) -> show n ++ "\t=\t" ++ show d) defs
