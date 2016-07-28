module CircleTreeCalculus(
        LCalc(..),
        Variable(..),
        (?),
        x, y, z, w, t, a, b, c,
        lambda
    ) where

data Variable = X | Y | Z | W | T | A | B | C deriving (Show)

data LCalc = Var Variable | Lambda Variable LCalc | App LCalc LCalc deriving (Show)

lambda :: [Variable] -> LCalc -> LCalc
lambda = foldr ((.) . Lambda) id

infixl 9 ?
(?) :: LCalc -> LCalc -> LCalc
(?) = App

x, y, z, w, t, a, b, c :: LCalc
[x, y, z, w, t, a, b, c] = map Var [X, Y, Z, W, T, A, B, C]