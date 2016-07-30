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

lambdaPos :: ReductionStep ->  P2 Double
lambdaPos s = p2 (xPos s, 0)

infixr 5 :--:
infixr 5 :->:
data Reduction = LCalc :--: Reduction | LCalc :->: Reduction | Final LCalc

drawReduction :: Reduction -> Diagram B
drawReduction = renderObjects' FirstStep
    where
    renderObjects' pos (Final term) = position [(lambdaPos pos, drawLambda term # named pos # center)]
    renderObjects' pos (term :->: rest) = twoSegs pos "Burst" term rest
    renderObjects' pos (term :--: rest) = twoSegs pos "Recolor" term rest
    twoSegs pos label term rest
            = position [(lambdaPos pos, lambImage), (textPos, textLabel)]
                <> renderObjects' (NextStep pos) rest
                <> arrowBetween arrowStartPos arrowEndPos
        where
        lambImage = drawLambda term # named pos # center
        textLabel = text label # fontSizeL 0.25
        textPos = p2 (xPos pos + 2, 0.25)
        arrowStartPos = p2 (xPos pos + 1.25, 0)
        arrowEndPos = p2 (xPos pos + 2.75, 0)
