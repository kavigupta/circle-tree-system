
import CircleTreeCalculus
import CircleTreeOutput

main :: IO ()
main = writeToFile "eg/y-combinator.svg" 400 $ Lambda X (value ? value)
    where value = Lambda Y (x ? (y ? y))

-- identity :: LCalc
-- identity = Lambda "x" x
