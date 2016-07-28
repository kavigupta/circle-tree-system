
import CircleTreeCalculus
import CircleTreeOutput

main :: IO ()
main = writeToFile "y-combinator.svg" 400 $ Lambda "x" (Lambda "y" (x $$ (y $$ y)) $$ Lambda "y" (x $$ (y $$ y)))

-- identity :: LCalc
-- identity = Lambda "x" x
