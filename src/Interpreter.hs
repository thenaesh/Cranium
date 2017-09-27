module Interpreter
    ( tokenize
    , execute
    ) where

import Data.Int
import Data.IORef
import Data.Array
import Data.Array.MArray
import Data.Array.IO

import Types


tokenize :: Char -> Instruction
tokenize c = case c of
    '>' -> MOVR
    '<' -> MOVL
    '+' -> INC
    '-' -> DEC
    '.' -> WRITE
    ',' -> READ
    '[' -> JMPF
    ']' -> JMPB
    err -> error $ "Invalid input \'" ++ [err] ++ "\' encountered."

execute :: ProgramState -> IO ()
execute programState@(program, tape, ip, dp) = do
     ip' <- readIORef ip
     dp' <- readIORef dp
     hasProgramTerminated <- return $  let (_, ub) = bounds program in ip' > ub
     isDataPointerOutOfBounds <- fmap (\(lb, ub) -> lb <= dp' && dp' <= ub) $ getBounds tape
     case (hasProgramTerminated, isDataPointerOutOfBounds) of
         (True, _) -> return ()
         (False, True) -> error "Data pointer went out of bounds."
         _ -> do
             apply programState $ program ! ip'
             execute programState

apply :: ProgramState -> Instruction -> IO ()
apply = undefined
