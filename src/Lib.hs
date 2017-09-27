module Lib
    ( readProgram
    , runProgram
    ) where

import Data.Int
import Data.IORef
import Data.Array
import Data.Array.MArray
import Data.Array.IO

import Types
import Interpreter


readProgram :: IO Program
readProgram = do
    programSource <- fmap (fmap tokenize) getLine
    return $ listArray (0, length programSource - 1) programSource

runProgram :: Program -> IO ()
runProgram program = do
    tape <- newArray (0, tapeLength - 1) 0 :: IO Tape
    instructionPointer <- newIORef (0 :: Int)
    dataPointer <- newIORef (0 :: Int)
    execute (program, tape, instructionPointer, dataPointer)
