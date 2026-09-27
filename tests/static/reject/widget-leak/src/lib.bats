#include "share/atspre_staload.hats"
#use widget as W

(* A widget tree is linear: one that is neither returned nor freed is
   leaked, which does not type-check *)
fn leak (): void = let
  val w = $W.Element($W.ElementNode($W.Root(), $W.Normal($W.Div()), $W.NoClass(), false,
    $W.NoneInt(), $W.NoneStr(), $W.WNil()))
in () end
