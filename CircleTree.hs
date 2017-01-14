import Control.Monad

import System.Directory

import CircleTreeGraphics
import CircleTreeCalculus
import CircleTreeOutput
import CircleTreeReductions

main :: IO ()
main = do
    introArticle
    arithmeticArticle

introArticle :: IO ()
introArticle = do
    createDirectoryIfMissing True "intro"
    "intro/vars.svg" <<< [x, y, z, t, w]
    "intro/lambdas.svg" <<< [Lambda X x, lambda [X, Y, Z] y, lambda [Y, X] x, lambda [X, X] x]
    "intro/apps.svg" <<< [x ? x, x ? y ? z, lambda [X] (lambda [X] (x ? lambda [Z] z) ? (y ? z))]
    "intro/tree.svg" <<< ((x ? y) ? (z ? x)) ? ((x ? y) ? (z ? x))
    "intro/red-inside-red.svg" <<< lambda [X] x
    "intro/red-inside-blue.svg" <<< lambda [Z] x
    "intro/contained-threedots.svg" <<< lambda [X, Y] (x ? y ? z)
    "intro/contained-outside.svg" <<< lambda [X] x ? x
    "intro/contained-two-same-color.svg" <<< lambda [X] (lambda [X] x ? x)
    "intro/contained-exercises.svg" <<< vert [
            lambda [X] (x ? lambda [Y, Z] (y ? (z ? lambda [Y] (y ? t))) ? lambda [Z] (y ? z)),
            lambda [X] (x ? y) ? lambda [Y] (x ? y)
        ]
    "intro/recolor-example.svg" <<<
        lambda [X, Y] $ x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)
    "intro/recolor-example-didnt-change-contained.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |-!-| lambda [X, T] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y))
    "intro/recolor-example-changed-contained-in-other.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |-!-| lambda [X, T] (x ? t ? lambda [Z] z ? (lambda [W] (w ? t) ? lambda [Y] t))
    "intro/recolor-example-correct-to-yellow.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |--| lambda [X, T] (x ? t ? lambda [Z] z ? (lambda [W] (w ? t) ? lambda [Y] y))
    "intro/recolor-example-capture-locally-free.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |-!-| lambda [X, X] (x ? x ? lambda [Z] z ? (lambda [W] (w ? x) ? lambda [Y] y))
    "intro/recolor-example-correct-to-blue.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |--| lambda [X, Z] (x ? z ? lambda [Z] z ? (lambda [W] (w ? z) ? lambda [Y] y))
    "intro/recolor-captured-by-bubble.svg" <<<
        lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) |-!-| lambda [X, W] (x ? w ? lambda [Z] z ? (lambda [W] (w ? w) ? lambda [Y] y))
    "intro/recolor-questions.svg" <<< vert [
            x ? lambda [X, Y] (x ? lambda [X] x) |-?-| x ? lambda [Y, Y] (y ? lambda [X] x),
            lambda [X] (x ? y) |-?-| lambda [Z] (z ? y),
            x ? y |-?-| z ? y,
            lambda [X, X] x |-?-| lambda [Y, X] y
        ]
    "intro/burst1.svg" <<<
        lambda [X, Z] (x ? z) ? y --> lambda [Z] (y ? z)
    "intro/burst-invalid1.svg" <<<
        lambda [X, Z] (x ? z) ? z -!-> lambda [Z] (z ? z)
    "intro/burst2.svg" <<<
        lambda [X, Y] x ? lambda [Z] z --> lambda [Y, Z] z
    "intro/burst3.svg" <<<
        lambda [X, Y] (x ? lambda [Z] x) ? (w ? t) --> lambda [Y] ((w ? t) ? lambda [Z] (w ? t))
    "intro/reduce1.svg" <<<
        lambda [X, Z] (x ? z) ? z |--| lambda [X, Y] (x ? y) ? z --> lambda [Y] (z ? y)
    "intro/pattern-impl.svg" <<< vert [
        (x ? y) ? (x ? z),
        (lambda [Y] y ? y) ? (lambda [Y] y ? z),
        (lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) ? y) ? (lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) ? z)]
    "intro/pattern-abs.svg" <<< vert [
        (x ? y) ? (x ? z) <-- lambda [X] ((x ? y) ? (x ? z)) ? x,
        (lambda [Y] y ? y) ? (lambda [Y] y ? z) <-- lambda [X] ((x ? y) ? (x ? z)) ? lambda [Y] y,
        (lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) ? y) ? (lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y)) ? z)
            <-- lambda [X] ((x ? y) ? (x ? z)) ? lambda [X, Y] (x ? y ? lambda [Z] z ? (lambda [W] (w ? y) ? lambda [Y] y))]
    "intro/pattern.svg" <<< lambda [X] ((x ? y) ? (x ? z))
    "intro/create1.svg" <<<
        x ? lambda [Y, Z] y --< lambda [Z] (x ? lambda [Y, Z] y ? z)
    "intro/create-invalid.svg" <<<
        x ? lambda [Y, Z] y -!-< lambda [X] (x ? lambda [Y, Z] y ? x)
    "intro/create-burst.svg" <<<
        lambda [X] (x ? lambda [Y, Z] y) --< lambda [Z] (lambda [X] (x ? lambda [Y, Z] y) ? z) --> lambda [Z] (z ? lambda [Y, Z] y) |--| lambda [X] (x ? lambda [Y, Z] y)

arithmeticArticle :: IO ()
arithmeticArticle = do
    forM_ [0, 1, 2, 3, 13] $ \n ->
        ("calc/" ++ show n ++ ".svg") <<< church X Y n
    "calc/plus.svg" <<<
        lambda [Z, W, T, B] ((z ? t) ? (w ? t ? b))
    "calc/plus-5-3.svg" <<<
        lambda [Z, W, T, B] ((z ? t) ? (w ? t ? b)) ? church X Y 5 ? church X Y 3
    "calc/plus-5-3-red1.svg" <<<
        lambda [Z, W, T, B] ((z ? t) ? (w ? t ? b)) ? church X Y 5 ? church X Y 3
            --> lambda [T, B] ((church X Y 5 ? t) ? (church X Y 3 ? t ? b))
    "calc/plus-5-3-red2.svg" <<<
        lambda [T, B] ((church X Y 5 ? t) ? (church X Y 3 ? t ? b))
            --> lambda [T, B] (lambda [Y] (t ? (t ? (t ? (t ? (t ? y))))) ? (lambda [Y] (t ? (t ? (t ? y))) ? b))
    "calc/plus-5-3-red3.svg" <<<
        lambda [T, B] (lambda [Y] (churchBody t y 5) ? (lambda [Y] (churchBody t y 3) ? b))
            --> lambda [T, B] (lambda [Y] (churchBody t y 5) ? churchBody t b 3)
            --> church T B 8
    "calc/plus-2-2-2-2.svg" <<<
        let two = (lambda [Z] (churchBody x z 2) ?) in
            lambda [X, Y] . two . two . two . two $ y
    "calc/plus-2-2-2-2-abs.svg" <<<
        let two = (lambda [Z] (churchBody x z 2)) in
            lambda [X] $ lambda [A] (lambda [Y] . (a ?) . (a ?) . (a ?) . (a ?) $ y) ? two
    "calc/plus-2-2-2-2-abs2.svg" <<<
        lambda [X] $ church A Y 4 ? (lambda [Y, Z] (churchBody y z 2) ? x)
    "calc/plus-2-2-2-2-abs3.svg" <<<
        lambda [Y, Z, X] (y ? (z ? x)) ? church Y Z 4 ? church Y Z 2
    "calc/mult-2-2-2-2.svg" <<<
        lambda [X] $ churchBody (church Y Z 2) x 4
    "calc/mult-2-2-2-2-abs.svg" <<<
        church T X 4 ? church Y Z 2
    "calc/exp-2-4.svg" <<<
        (lambda [Z, A] $ a ? z) ? church X Y 2 ? church X Y 4

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
