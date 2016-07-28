
import CircleTreeCalculus
import CircleTreeOutput

main :: IO ()
main = do
    writeToFile "eg/y-combinator.svg" 400 yCombine
    writeToFile "eg/identity.svg" 400 identity

identity :: LCalc
identity = Lambda X x


yCombine :: LCalc
yCombine = Lambda X (value ? value)
    where value = Lambda Y (x ? (y ? y))