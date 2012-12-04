Marked text provides a datastructure that associates marked intervals with text,
such that marks survive editing.  These marks change how the text is rendered
and also have behaviors (in the form of a typeclass instance) which determine
how they behave during text editing.

It was originally used in https://github.com/mgsloan/panopti, and now its more
recent, less scope-ey descendent https://github.com/mgsloan/diagrams-ghci .
It used to reside in https://github.com/mgsloan/gtk-toy-diagrams .

Ideally this would use a btree / fingertree, and distribute the marks on the
inner nodes, but that's pending it being used for something that actually
demands performance.  Until then, it provides a very naive implementation.


```haskell
-- A keyhandler, for use with @gtk-toy-diagrams@, which lets the user type
-- characters, use the arrows, delete, and backspace. 
textKeyHandler :: (Eq m, Mark m, CanBeCursor m)
               => KeyEvent -> MarkedText m -> MarkedText m
textKeyHandler (True, ev) mt = case ev of
  Right k -> insert [k]
  Left  k -> case k of
    "Return"    -> insert "\n"
    "Left"      -> mutateCursors (subtract 1)
    "Right"     -> mutateCursors (+1)
    "Home"      -> mutateCursors (const (-maxIx, -maxIx))
    "End"       -> mutateCursors (const (maxIx, maxIx))
    "Delete"    -> editCursors (\(ivl, _) -> (second  (+1)  ivl, cursorText))
    "BackSpace" -> editCursors (\(ivl, _) -> (first (+(-1)) ivl, cursorText))
    _           -> mt
 where
  editCursors f = edit (whenMarked isCursor f) mt

  insert s = editCursors $ second $ const (MarkedText s [((p, p), mkCursor)])
    where p = length s

  mutateCursors f
    = mutateMarks ( \(i, m) -> Just (if isCursor m then f i else i, m) ) mt

  maxIx = textLength mt
```
