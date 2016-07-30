import Diagrams.Backend.SVG

import Diagrams.Prelude

import CircleTreeGraphics
import CircleTreeCalculus
import CircleTreeOutput
import CircleTreeReductions

main :: IO ()
main = do
    renderSVG "eg/vars.svg" (mkHeight 400) . hsep 2 . fmap drawLambda $ [x, y, z, t, w]
    renderSVG "eg/lambdas.svg" (mkHeight 400) . hsep 2 . fmap drawLambda $ [Lambda X x, lambda [X, Y, Z] y, lambda [Y, X] x, lambda [X, X] x]
    renderSVG "eg/apps.svg" (mkHeight 600) . hsep 2 . fmap drawLambda $ [x ? x, x ? y ? z, lambda [X] (lambda [X] (x ? lambda [Z] z) ? (y ? z))]
    renderSVG "eg/tree.svg" (mkHeight 600) . drawLambda $ ((x ? y) ? (z ? x)) ? ((x ? y) ? (z ? x))
    -- renderSVG "eg/recolor.svg" (mkHeight 600) . hsep 2 .  fmap drawLambda $ [lambda [x]]
    writeToFile "eg/var.svg" 400 x
    writeToFile "eg/y-combinator.svg" 400 yCombine
    writeToFile "eg/id.svg" 400 identity
    renderSVG "example.svg" (mkHeight 400) . drawReduction $ yCombine :--: yCombine :->: Final yCombine

identity :: LCalc
identity = Lambda X x

yCombine :: LCalc
yCombine = Lambda X (omega ? omega)
    where omega = Lambda Y (x ? (y ? y))
