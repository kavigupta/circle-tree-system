module CircleTreeReductions(
        (|--|),  (-->),  (<--),  (--<),
        (|-?-|), (-?->), (<-?-), (-?-<),
        (|-!-|), (-!->), (<-!-), (-!-<)
    ) where

import Diagrams.Prelude
import CircleTreeCalculus
import Diagrams.Backend.SVG.CmdLine
import CircleTreeGraphics
import CircleTreeHelpers

infixr 5 |--|, -->, <--, --<, |-?-|, -?->, <-?-, -?-<, |-!-|, -!->, <-!-, -!-<

(|--|), (-->), (<--), (--<), (|-?-|), (-?->), (<-?-), (-?-<), (|-!-|), (-!->), (<-!-), (-!-<) :: LCalc -> rest -> RedCT rest
(|--|) = RedCT "Recolor" LR Valid
(|-?-|) = RedCT "Recolor" LR Question
(|-!-|) = RedCT "Recolor" LR Invalid
(-->) = RedCT "Burst" R Valid
(-?->) = RedCT "Burst" R Question
(-!->) = RedCT "Burst" R Invalid
(<--) = RedCT "Burst" L Valid
(<-?-) = RedCT "Burst" L Question
(<-!-) = RedCT "Burst" L Invalid
(--<) = RedCT "Create" R Valid
(-?-<) = RedCT "Create" R Question
(-!-<) = RedCT "Create" R Invalid

data Dir = L | R | LR

data IsValid = Valid | Invalid | Question

superposeArrow :: IsValid -> Diagram B -> Diagram B
superposeArrow Valid = id
superposeArrow Invalid = invalid
superposeArrow Question = questionable

data RedCT rest = RedCT String Dir IsValid LCalc rest

arrowDir :: Dir -> Point V2 Double -> Point V2 Double -> Diagram B
arrowDir L = flip arrowBetween
arrowDir R = arrowBetween
arrowDir LR = liftA2 (<>) (arrowDir L) (arrowDir R)

instance (DrawCT rest) => DrawCT (RedCT rest) where
    drawCT (RedCT label dir isvalid term rest)
        = lambImage # translate ((2 - width lambImage) / 2 ^& 0)
            <> drawCT rest # moveTo nextPos
            <> arrowDir dir arrowStartPos arrowEndPos # superposeArrow isvalid
            <> textLabel # moveTo textPos
        where
        lambImage = drawCT term
        textLabel = text label # fontSizeL 0.25
        textPos = p2 (2, 0.25)
        arrowStartPos = p2 (1.25, 0)
        arrowEndPos = p2 (2.75, 0)
        nextPos = p2 (4, 0)


-- class ReductionCT r where

-- instance (DrawCT to) => DrawCT (red, to) where
--     drawCT (_, _) = undefined
