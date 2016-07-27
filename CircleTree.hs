
import Diagrams.Backend.SVG.CmdLine

import CircleTreeCalculus

main :: IO ()
main = mainWith $ drawLambda $ Lambda "x" (Lambda "y" (x $$ (y $$ y)) $$ Lambda "y" (x $$ (y $$ y)))

-- identity :: LCalc
-- identity = Lambda "x" x
