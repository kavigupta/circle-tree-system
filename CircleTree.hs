import Control.Monad

import CircleTreeGraphics
import CircleTreeCalculus
import CircleTreeOutput
import CircleTreeReductions

main :: IO ()
main = do
    introArticle
    arithmeticArticle

introArticle :: IO ()
introArticle = runOutput "intro" $ do
    "vars.svg" <<< [x, y, z, t, w]
    "lambdas.svg" <<< [Lambda X x, lambda [X, Y, Z] y, lambda [Y, X] x, lambda [X, X] x]
    "apps.svg" <<< [x ? x, x ? y ? z, lambda [X] (lambda [X] (x ? lambda [Z] z) ? (y ? z))]
    "tree.svg" <<< ((x ? y) ? (z ? x)) ? ((x ? y) ? (z ? x))
    "red-inside-red.svg" <<< lambda [X] x
    "red-inside-blue.svg" <<< lambda [Z] x
    "contained-threedots.svg" <<< lambda [X, Y] (x ? y ? z)
    "contained-outside.svg" <<< lambda [X] x ? x
    "contained-two-same-color.svg" <<< lambda [X] (lambda [X] x ? x)
    "contained-exercises.svg" <<< vert [
            lambda [X] (x ? lambda [Y, Z] (y ? (z ? lambda [Y] (y ? t))) ? lambda [Z] (y ? z)),
            lambda [X] (x ? y) ? lambda [Y] (x ? y)
        ]
    "recolor-example.svg" <<<
        lambda [X, Y] $ x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)
    "recolor-example-didnt-change-contained.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |-!-| lambda [X, T] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y))
    "recolor-example-changed-contained-in-other.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |-!-| lambda [X, T] (x ? t ? lambda [Z] z ? (lambda [W] (w ? t) ? lambda [Y] t))
    "recolor-example-correct-to-yellow.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |--| lambda [X, T] (x ? t ? lambda [Z] z ? (lambda [W] (w ? t) ? lambda [Y] y))
    "recolor-example-capture-locally-free.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |-!-| lambda [X, X] (x ? x ? lambda [Z] z ? (lambda [W] (w ? x) ? lambda [Y] y))
    "recolor-example-correct-to-blue.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |--| lambda [X, Z] (x ? z ? lambda [Z] z ? (lambda [W] (w ? z) ? lambda [Y] y))
    "recolor-captured-by-bubble.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |-!-| lambda [X, W] (x ? w ? lambda [Z] z ? (lambda [W] (w ? w) ? lambda [Y] y))
    "recolor-questions.svg" <<< vert [
            x ? lambda [X, Y] (x ? lambda [X] x) |-?-| x ? lambda [Y, Y] (y ? lambda [X] x),
            lambda [X] (x ? y) |-?-| lambda [Z] (z ? y),
            x ? y |-?-| z ? y,
            lambda [X, X] x |-?-| lambda [Y, X] y
        ]
    "burst1.svg" <<<
        lambda [X, Z] (x ? z) ? y --> lambda [Z] (y ? z)
    "burst-invalid1.svg" <<<
        lambda [X, Z] (x ? z) ? z -!-> lambda [Z] (z ? z)
    "burst2.svg" <<<
        lambda [X, Y] x ? lambda [Z] z --> lambda [Y, Z] z
    "burst3.svg" <<<
        lambda [X, Y] (x ? lambda [Z] x) ? (w ? t) --> lambda [Y] ((w ? t) ? lambda [Z] (w ? t))
    "reduce1.svg" <<<
        lambda [X, Z] (x ? z) ? z |--| lambda [X, Y] (x ? y) ? z --> lambda [Y] (z ? y)
    "pattern-impl.svg" <<< vert [
        (x ? y) ? (x ? z),
        (lambda [Y] y ? y) ? (lambda [Y] y ? z),
        (lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) ? y) ? (lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) ? z)]
    "pattern-abs.svg" <<< vert [
        (x ? y) ? (x ? z) <-- lambda [X] ((x ? y) ? (x ? z)) ? x,
        (lambda [Y] y ? y) ? (lambda [Y] y ? z) <-- lambda [X] ((x ? y) ? (x ? z)) ? lambda [Y] y,
        (lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) ? y) ? (lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) ? z)
            <-- lambda [X] ((x ? y) ? (x ? z)) ? lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y))]
    "pattern.svg" <<< lambda [X] ((x ? y) ? (x ? z))
    "create1.svg" <<<
        x ? lambda [Y, Z] y --< lambda [Z] (x ? lambda [Y, Z] y ? z)
    "create-invalid.svg" <<<
        x ? lambda [Y, Z] y -!-< lambda [X] (x ? lambda [Y, Z] y ? x)
    "create-burst.svg" <<<
        lambda [X] (x ? lambda [Y, Z] y) --< lambda [Z] (lambda [X] (x ? lambda [Y, Z] y) ? z) --> lambda [Z] (z ? lambda [Y, Z] y) |--| lambda [X] (x ? lambda [Y, Z] y)

arithmeticArticle :: IO ()
arithmeticArticle = runOutput "calc" $ do
    forM_ [0, 1, 2, 3, 13] $ \n ->
        (show n ++ ".svg") <<< church X Y n
    "plus.svg" <<<
        lambda [Z, X, A, Y] ((z ? a) ? (x ? a ? y))
    "plus-5-3.svg" <<<
        lambda [Z, X, A, Y] ((z ? a) ? (x ? a ? y)) ? church X Z 5 ? church X Z 3
    "plus-5-3-red1.svg" <<<
        lambda [Z, X, A, Y] ((z ? a) ? (x ? a ? y)) ? church X Z 5 ? church X Z 3
            --> lambda [A, Y] ((church X Z 5 ? a) ? (church X Z 3 ? a ? y))
    "plus-5-3-red2.svg" <<<
        lambda [A, Y] ((church X Z 5 ? a) ? (church X Z 3 ? a ? y))
            --> lambda [A, Y] (lambda [Z] (churchBody a z 5) ? (lambda [Z] (churchBody a z 3) ? y))
    "plus-5-3-red3.svg" <<<
        lambda [A, Y] (lambda [Z] (churchBody a z 5) ? (lambda [Z] (churchBody a z 3) ? y))
            --> lambda [A, Y] (lambda [Z] (churchBody a z 5) ? churchBody a y 3)
            --> church A Y 8
    "plus-2-2-2-2.svg" <<<
        let two = (lambda [Z] (churchBody x z 2) ?) in
            lambda [X, Y] . two . two . two . two $ y
    "plus-2-2-2-2-abs.svg" <<<
        let two = lambda [Z] (churchBody x z 2) in
            lambda [X] $ lambda [A] (lambda [Y] . (a ?) . (a ?) . (a ?) . (a ?) $ y) ? two
    "plus-2-2-2-2-abs2.svg" <<<
        lambda [X] $ church A Y 4 ? (lambda [Y, Z] (churchBody y z 2) ? x)
    "plus-2-2-2-2-abs3.svg" <<<
        lambda [Y, Z, X] (y ? (z ? x)) ? church Y Z 4 ? church Y Z 2
    "mult-2-2-2-2.svg" <<<
        lambda [X] $ churchBody (church Y Z 2) x 4
    "mult-2-2-2-2-abs.svg" <<<
        church T X 4 ? church Y Z 2
    "exp-2-4.svg" <<<
        lambda [Z, A] (a ? z) ? church X Y 2 ? church X Y 4

church :: Variable -> Variable -> Int -> LCalc
church v1 v2 = lambda [v1, v2] . churchBody (Var v1) (Var v2)

churchBody :: LCalc -> LCalc -> Int -> LCalc
churchBody _ v2 0 = v2
churchBody v1 v2 n = v1 ? churchBody v1 v2 (n - 1)
-- identity :: LCalc
-- identity = Lambda X x
--
-- yCombine :: LCalc
-- yCombine = Lambda X (omega ? omega)
--     where omega = Lambda Y (x ? (y ? y))
