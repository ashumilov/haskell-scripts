module Main where

data Env = Env { s :: String, i :: Int }

type App = (->) Env

app :: App Char
app (Env s i) = s !! i

main :: IO ()
main = print $ app (Env "reader" 3)
