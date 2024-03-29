module Toys.Text where

import Data.Default
import Graphics.UI.Toy.Prelude
import Graphics.UI.Toy.Text

type State = MarkedText CursorMark

main :: IO ()
main = runToy (def :: MarkedText CursorMark)

instance (Mark a, CanBeCursor a) => Default (MarkedText a) where
  def = cursorText