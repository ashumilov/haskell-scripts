{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Reader 
import Control.Monad.State 
import Control.Monad.Writer 
import Control.Exception (assert)

-- Let's define a virtual machine equipped with a stack that supports 4 operations: Push, Pop, Add, and Puts
-- Push: push an integer into the stack
-- Pop: pop the top of the stack
-- Add: pop the top 2 integers, add them, and push into the stack
-- Puts: peek the integer on the top of the stack and add it to the output list.
-- The virtual machine takes a list of these instructions, execute it, and return a list. 
-- Can you represent this virtual machine?
-- You can use hoogle

data Instr = Push Int | Pop | Add | Puts

type Stack = [Int]

type Program = [Instr]

type Output = [Int]

-- you can define your types here.

execVM :: Program -> Output
execVM = error "your code"

program1 :: Program
program1 =
  [ Push 2,
    Push 3,
    Add,
    Puts,
    Push 4,
    Push 5,
    Add,
    Puts,
    Push 1,
    Puts,
    Puts
  ]

-- above program should output [5, 9, 1, 1]
main :: IO ()
main = do
  let results1 = execVM program1
  assert (results1 == [5, 9, 1, 1]) $ do print "1 - ok"
