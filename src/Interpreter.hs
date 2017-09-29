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
    '.' -> PRINT
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
apply (program, tape, ip, dp) instruction = do
    case instruction of
        NOPE -> do
            ip' <- readIORef ip
            hPutStrLn stderr $ "WARNING: Invalid character encountered at position " ++ show ip' ++ "!"
        MOVR -> moveDataPointer dp 1
        MOVL -> moveDataPointer dp (-1)
        INC -> modifyTapeData tape dp 1
        DEC -> modifyTapeData tape dp (-1)
        PRINT -> readTapeData tape dp >>= (putChar . toEnum . fromEnum)
        READ -> (toEnum . fromEnum) <$> getChar >>= writeTapeData tape dp
        JMPF -> do
            val <- readTapeData tape dp
            case val of
                0 -> advanceToMatchingBracket program ip
                _ -> return ()
        JMPB -> do
            val <- readTapeData tape dp
            case val of
                0 -> return ()
                _ -> rewindToMatchingBracket program ip
    moveInstructionPointer ip 1

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

advanceToMatchingBracket :: Program -> InstructionPointer -> IO ()
advanceToMatchingBracket program ip = f 1
    where
        f :: Int -> IO ()
        f 0 = return ()
        f n = do
            modifyIORef ip (\x -> x + 1)
            instruction <- fmap (program !) $ readIORef ip
            let n' = case instruction of
                    JMPF -> n + 1
                    JMPB -> n - 1
                    _ -> n
            f n'

rewindToMatchingBracket :: Program -> InstructionPointer -> IO ()
rewindToMatchingBracket program ip = f 1
    where
        f :: Int -> IO ()
        f 0 = return ()
        f n = do
            modifyIORef ip (\x -> x - 1)
            instruction <- fmap (program !) $ readIORef ip
            let n' = case instruction of
                    JMPB -> n + 1
                    JMPF -> n - 1
                    _ -> n
            f n'

tapeLength :: Int
tapeLength = 30000

tapeInitialValue :: Int8
tapeInitialValue = 0
