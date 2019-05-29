module Nuri.Spec.Util where

import           Text.Megaparsec.Pos

initPos :: SourcePos
initPos = initialPos "(test)"

newPos :: Int -> Int -> SourcePos
newPos p1 p2 = SourcePos "(test)" (mkPos p1) (mkPos p2)
