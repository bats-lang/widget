#include "share/atspre_staload.hats"
#use array as A
#use str as S
#use widget as W

fn div (): $W.widget =
  $W.Element($W.ElementNode($W.Root(), $W.Normal($W.Div()), $W.NoClass(), false, $W.NoneInt(), $W.NoneStr(), $W.WNil()))

fn last (): $W.diff = let
  val @(w, d) = $W.set_class(div(), 675)
  val () = $W.widget_free(w)
in d end
