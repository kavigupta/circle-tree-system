module CircleTreeCalculus(
        LCalc(..),
        ($$),
        x, y, z, w, t
    ) where

data LCalc = Var String | Lambda String LCalc | App LCalc LCalc

infixl 9 $$
($$) :: LCalc -> LCalc -> LCalc
($$) = App

x, y, z, w, t :: LCalc
[x, y, z, w, t] = map (Var . return) "xyzwt"