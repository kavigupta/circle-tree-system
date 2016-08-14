import Control.Monad

import CircleTreeGraphics
import CircleTreeCalculus
import CircleTreeOutput
import CircleTreeReductions

main :: IO ()
main = do
    "eg/vars.svg" <<< [x, y, z, t, w]
    "eg/lambdas.svg" <<< [Lambda X x, lambda [X, Y, Z] y, lambda [Y, X] x, lambda [X, X] x]
    "eg/apps.svg" <<< [x ? x, x ? y ? z, lambda [X] (lambda [X] (x ? lambda [Z] z) ? (y ? z))]
    "eg/tree.svg" <<< ((x ? y) ? (z ? x)) ? ((x ? y) ? (z ? x))
    "eg/red-inside-red.svg" <<< lambda [X] x
    "eg/red-inside-blue.svg" <<< lambda [Z] x
    "eg/contained1.svg" <<< lambda [X, Y] (x ? y ? z)
    "eg/contained2.svg" <<< lambda [X] x ? x
    "eg/contained3.svg" <<< lambda [X] (lambda [X] x ? x)
    "eg/contained-exercise1.svg" <<<
        lambda [X] (x ? lambda [Y, Z] (y ? (z ? lambda [Y] (y ? t))) ? lambda [Z] (y ? z))
    "eg/contained-exercise2.svg" <<<
        lambda [X] (x ? y) ? lambda [Y] (x ? y)
    "eg/recolor-example.svg" <<<
        lambda [X, Y] $ x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)
    "eg/recolor-example-didnt-change-contained.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |-!-| lambda [X, B] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y))
    "eg/recolor-example-changed-contained-in-other.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |-!-| lambda [X, T] (x ? t ? lambda [Z] z ? (lambda [W] (w ? t) ? lambda [Y] t))
    "eg/recolor-example-correct-to-yellow.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |--| lambda [X, T] (x ? t ? lambda [Z] z ? (lambda [W] (w ? t) ? lambda [Y] y))
    "eg/recolor-example-capture-locally-free.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |-!-| lambda [X, X] (x ? x ? lambda [Z] z ? (lambda [W] (w ? x) ? lambda [Y] y))
    "eg/recolor-example-correct-to-blue.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |--| lambda [X, Z] (x ? z ? lambda [Z] z ? (lambda [W] (w ? z) ? lambda [Y] y))
    "eg/recolor-captured-by-bubble.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |-!-| lambda [X, W] (x ? w ? lambda [Z] z ? (lambda [W] (w ? w) ? lambda [Y] y))
    "eg/recolor1.svg" <<<
        lambda [X, Z] (x ? z) |--| lambda [Y, Z] (y ? z)
    "eg/recolor-invalid1.svg" <<<
        lambda [X, Z] (x ? z) |-!-| lambda [Z, Z] (z ? z)
    "eg/recolor-invalid2.svg" <<<
        lambda [Z, X] (x ? z) |-!-| lambda [Z, Z] (z ? z)
    "eg/recolor-invalid2.svg" <<<
        lambda [Z, X] (x ? z) |-!-| lambda [Z, Z] (z ? z)
    "eg/recolor2.svg" <<<
        x ? lambda [X] x |--| x ? lambda [Y] y
    "eg/recolor3.svg" <<<
        x ? lambda [X] (x ? lambda [Z] (x ? lambda [X] x)) |--| x ? lambda [Y] (y ? lambda [Z] (y ? lambda [X] x))
    "eg/recolor-questions.svg" <<< vert [
            x ? lambda [X, Y] (x ? lambda [X] x) |-?-| x ? lambda [Y, Y] (y ? lambda [X] x),
            lambda [X] (x ? y) |-?-| lambda [Z] (z ? y),
            x ? y |-?-| z ? y,
            lambda [X, X] x |-?-| lambda [Y, X] y
        ]
    "eg/burst1.svg" <<<
        lambda [X, Z] (x ? z) ? y --> lambda [Z] (y ? z)
    "eg/burst-invalid1.svg" <<<
        lambda [X, Z] (x ? z) ? z -!-> lambda [Z] (z ? z)
    "eg/burst2.svg" <<<
        lambda [X, Y] x ? lambda [Z] z --> lambda [Y, Z] z
    "eg/burst3.svg" <<<
        lambda [X, Y] (x ? lambda [Z] x) ? (w ? t) --> lambda [Y] ((w ? t) ? lambda [Z] (w ? t))
    "eg/reduce1.svg" <<<
        lambda [X, Z] (x ? z) ? z |--| lambda [X, Y] (x ? y) ? z --> lambda [Y] (z ? y)
    "eg/pattern-impls.svg" <<< vert [
        (x ? y) ? (x ? z),
        (lambda [Y] y ? y) ? (lambda [Y] y ? z),
        (lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) ? y) ? (lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) ? z)]
    "eg/pattern-abs.svg" <<< vert [
        (x ? y) ? (x ? z) <-- lambda [W] ((w ? y) ? (w ? z)) ? x,
        (lambda [Y] y ? y) ? (lambda [Y] y ? z) <-- lambda [W] ((w ? y) ? (w ? z)) ? lambda [Y] y,
        (lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) ? y) ? (lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) ? z)
            <-- lambda [W] ((w ? y) ? (w ? z)) ? lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y))]
    "eg/pattern.svg" <<< lambda [W] ((w ? y) ? (w ? z))
    "eg/create1.svg" <<<
        x ? lambda [Y, Z] y --< lambda [Z] (x ? lambda [Y, Z] y ? z)
    "eg/create-invalid.svg" <<<
        x ? lambda [Y, Z] y -!-< lambda [X] (x ? lambda [Y, Z] y ? x)
    "eg/create-burst.svg" <<<
        lambda [X] (x ? lambda [Y, Z] y) --< lambda [Z] (lambda [X] (x ? lambda [Y, Z] y) ? z) --> lambda [Z] (z ? lambda [Y, Z] y) |--| lambda [X] (x ? lambda [Y, Z] y)
    forM_ [0, 1, 2, 3, 13] $ \n ->
        ("eg/" ++ show n ++ ".svg") <<< church X Y n
    "eg/plus.svg" <<<
        lambda [Z, W, T, B] ((z ? t) ? (w ? t ? b))
    "eg/plus-5-3.svg" <<<
        lambda [Z, W, T, B] ((z ? t) ? (w ? t ? b)) ? church X Y 5 ? church X Y 3
    "eg/plus-5-3-red1.svg" <<<
        lambda [Z, W, T, B] ((z ? t) ? (w ? t ? b)) ? church X Y 5 ? church X Y 3
            --> lambda [T, B] ((church X Y 5 ? t) ? (church X Y 3 ? t ? b))
    "eg/plus-5-3-red2.svg" <<<
        lambda [T, B] ((church X Y 5 ? t) ? (church X Y 3 ? t ? b))
            --> lambda [T, B] (lambda [Y] (t ? (t ? (t ? (t ? (t ? y))))) ? (lambda [Y] (t ? (t ? (t ? y))) ? b))
    "eg/plus-5-3-red3.svg" <<<
        lambda [T, B] (lambda [Y] (churchBody t y 5) ? (lambda [Y] (churchBody t y 3) ? b))
            --> lambda [T, B] (lambda [Y] (churchBody t y 5) ? churchBody t b 3)
            --> church T B 8
    "eg/plus-2-2-2-2.svg" <<<
        let two = (lambda [Z] (churchBody x z 2) ?) in
            lambda [X, Y] . two . two . two . two $ y
    "eg/plus-2-2-2-2-abs2.svg" <<<
        lambda [X] $ church A Y 4 ? (lambda [Y, Z] (churchBody y z 2) ? x)
    "eg/plus-2-2-2-2-abs3.svg" <<<
        lambda [Y, Z, X] (y ? (z ? x)) ? church Y Z 4 ? church Y Z 2
    "eg/mult-2-2-2-2.svg" <<<
        lambda [X] $ churchBody (church Y Z 2) x 4
    "eg/mult-2-2-2-2-abs.svg" <<<
        church T X 4 ? church Y Z 2

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
