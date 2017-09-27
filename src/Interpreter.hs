module Interpreter
    ( tokenize
    , execute
    , tapeLength
    , tapeInitialValue
    ) where

import Data.Int
import Data.IORef
import Data.Array
import Data.Array.MArray
import Data.Array.IO
import System.IO (hPutStrLn, stderr)

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
    _   -> NOPE

execute :: ProgramState -> IO ()
execute programState@(program, tape, ip, dp) = do
     ip' <- readIORef ip
     dp' <- readIORef dp
     let hasProgramTerminated = let (_, ub) = bounds program in ip' > ub
     case hasProgramTerminated of
         True -> return ()
         False -> do
             apply programState $ program ! ip'
             execute programState

apply :: ProgramState -> Instruction -> IO ()
apply (program, tape, ip, dp) instruction = case instruction of
    NOPE -> do
        ip' <- readIORef ip
        hPutStrLn stderr $ "WARNING: Invalid character encountered at position " ++ show ip' ++ "!"
    MOVR -> moveDataPointer dp 1 >> moveInstructionPointer ip 1
    MOVL -> moveDataPointer dp (-1) >> moveInstructionPointer ip 1
    INC -> modifyTapeData tape dp 1 >> moveInstructionPointer ip 1
    DEC -> modifyTapeData tape dp (-1) >> moveInstructionPointer ip 1
    WRITE -> readTapeData tape dp >>= (putChar . toEnum . fromEnum)
    READ -> writeTapeData tape dp =<< (toEnum . fromEnum) <$> getChar

moveInstructionPointer :: InstructionPointer -> Int -> IO ()
moveInstructionPointer ip offset = modifyIORef' ip (+ offset)

moveDataPointer :: DataPointer -> Int -> IO ()
moveDataPointer dp offset = modifyIORef' dp (\n -> (offset + n) `mod` tapeLength)

modifyTapeData :: Tape -> DataPointer -> Int8 -> IO ()
modifyTapeData tape dp delta = do
    dp' <- readIORef dp
    writeArray tape dp' =<< (+ delta) <$> readArray tape dp'

readTapeData :: Tape -> DataPointer -> IO Int8
readTapeData tape dp = readIORef dp >>= readArray tape

writeTapeData :: Tape -> DataPointer -> Int8 -> IO ()
writeTapeData tape dp value = readIORef dp >>= (\dp' -> writeArray tape dp' value)

tapeLength :: Int
tapeLength = 30000

tapeInitialValue :: Int8
tapeInitialValue = 0
