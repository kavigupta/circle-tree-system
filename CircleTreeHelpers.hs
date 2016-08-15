module CircleTreeHelpers(invalid, questionable) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

invalid :: Diagram B -> Diagram B
invalid dia = (rect long short <> rect short long) # lw none # fc red # rotate (45 @@ deg) # alignToLoc dia <> dia
    where
    long :: Double
    long = max (width dia) (height dia)
    short = long / 30

alignToLoc :: Diagram B -> Diagram B -> Diagram B
alignToLoc alignTo dia = dia # translate (centerPoint alignTo .-. centerPoint dia)

questionable :: Diagram B -> Diagram B
questionable dia = (text "?" # fontSizeL (width dia) # lw none # fc purple) # alignToLoc dia # translateY (-width dia * 0.375) <> dia
