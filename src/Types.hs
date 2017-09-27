module Types
    ( Instruction (..)
    , Program
    , Tape
    , InstructionPointer
    , DataPointer
    , ProgramState
    ) where

import Data.Int
import Data.IORef
import Data.Array
import Data.Array.MArray
import Data.Array.IO


data Instruction = NOPE  -- invalid instruction
                 | MOVR  -- move data pointer right
                 | MOVL  -- move data pointer left
                 | INC   -- increment value stored at data pointer
                 | DEC   -- decrement value stored at data pointer
                 | WRITE -- write value stored at data pointer to STDOUT
                 | READ  -- read value stored at data pointer from STDIN
                 | JMPF  -- move instruction pointer to after matching JMPB if value at data pointer is zero
                 | JMPB  -- move instruction pointer to after matching JMPF if value at data pointer is nonzero

type Program = Array Int Instruction
type Tape = IOUArray Int Int8
type InstructionPointer = IORef Int
type DataPointer = IORef Int

type ProgramState = (Program, Tape, InstructionPointer, DataPointer)
