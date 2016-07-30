{-# LANGUAGE DeriveDataTypeable #-}
module CircleTreeReductions(
        Reduction(..),
        drawReduction
    ) where

import Diagrams.Prelude
import Data.Typeable
import CircleTreeCalculus
import Diagrams.Backend.SVG.CmdLine
import CircleTreeGraphics

data ReductionStep = FirstStep | NextStep ReductionStep deriving (Typeable, Eq, Ord, Show)
instance IsName ReductionStep

xPos :: ReductionStep -> Double
xPos FirstStep = 0
xPos (NextStep s) = xPos s + 4

lambdaPos, textPos :: ReductionStep ->  P2 Double
lambdaPos s = p2 (xPos s, 0)
textPos s = p2 (xPos s + 2, 0.25)

infixr 5 :--:
infixr 5 :->:
data Reduction = LCalc :--: Reduction | LCalc :->: Reduction | Final LCalc

drawReduction :: Reduction -> Diagram B
drawReduction lcalc = renderObjects lcalc # arrows (reductionLength lcalc)
    where
    reductionLength :: Reduction -> ReductionStep
    reductionLength (Final _) = FirstStep
    reductionLength (_ :->: rest) = NextStep (reductionLength rest)
    reductionLength (_ :--: rest) = NextStep (reductionLength rest)
    arrows :: ReductionStep -> Diagram B -> Diagram B
    arrows FirstStep = id
    arrows (NextStep s) = connectOutside' arrowStyle1 s (NextStep s) . arrows s
    renderObjects :: Reduction -> Diagram B
    renderObjects = renderObjects' FirstStep
        where
        renderObjects' pos (Final term) = position [(lambdaPos pos, drawLambda term # named pos)]
        renderObjects' pos (term :->: rest) = twoSegs pos "Burst" term rest
        renderObjects' pos (term :--: rest) = twoSegs pos "Recolor" term rest

        twoSegs pos label term rest
                = position [(lambdaPos pos, lambImage), (textPos pos, textLabel)]
                    <> renderObjects' (NextStep pos) rest
            where
            lambImage = drawLambda term # named pos
            textLabel = text label # fontSizeL 0.25

-- shaft  = arc xDir (-1/6 @@ turn)

arrowStyle1 :: ArrowOpts Double
arrowStyle1 = with  & arrowHead  .~ spike & headLength .~ normal
