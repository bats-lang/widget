#include "share/atspre_staload.hats"
#use widget as W

fn div (): $W.widget =
  $W.Element($W.ElementNode($W.Root(), $W.Normal($W.Div()), $W.NoClass(), false, $W.NoneInt(), $W.NoneStr(), $W.WNil()))

(* add_child consumes the child: it is in the parent now, and cannot be
   changed (or freed) on its own *)
fn reuse (): void = let
  val kid = div()
  val @(p, d) = $W.add_child(div(), kid)
  val () = $W.diff_free(d)
  val () = $W.widget_free(p)
in $W.widget_free(kid) end
