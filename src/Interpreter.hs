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
apply (program, tape, ip, dp) instruction = case instruction of
    MOVR -> moveDataPointer dp 1 >> moveInstructionPointer ip 1
    MOVL -> moveDataPointer dp (-1) >> moveInstructionPointer ip 1
    INC -> modifyTapeData tape dp 1 >> moveInstructionPointer ip 1
    DEC -> modifyTapeData tape dp (-1) >> moveInstructionPointer ip 1
    _ -> undefined

moveInstructionPointer :: InstructionPointer -> Int -> IO ()
moveInstructionPointer ip offset = modifyIORef' ip (+ offset)

moveDataPointer :: DataPointer -> Int -> IO ()
moveDataPointer dp offset = modifyIORef' dp (+ offset)

modifyTapeData :: Tape -> DataPointer -> Int8 -> IO ()
modifyTapeData tape dp delta = do
    dp' <- readIORef dp
    writeArray tape dp' =<< (+ delta) <$> readArray tape dp'
