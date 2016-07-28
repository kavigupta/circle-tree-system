
import CircleTreeCalculus
import CircleTreeOutput

main :: IO ()
main = writeToFile "y-combinator.svg" 400 $ Lambda X (Lambda Y (x $$ (y $$ y)) $$ Lambda Y (x $$ (y $$ y)))

-- identity :: LCalc
-- identity = Lambda "x" x
