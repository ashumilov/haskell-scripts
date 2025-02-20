{-# OPTIONS_GHC -Wall -fwarn-incomplete-uni-patterns #-}

import Control.Monad.State

test :: State Int Int
test = do
  put 3
  modify (1+)
  get

main :: IO ()
main = print $ runState test 0

