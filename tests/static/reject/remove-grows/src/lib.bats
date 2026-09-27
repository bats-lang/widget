#include "share/atspre_staload.hats"
#use array as A
#use str as S
#use widget as W

fn div (): $W.widget =
  $W.Element($W.ElementNode($W.Root(), $W.Normal($W.Div()), $W.NoClass(), 0, $W.NoneInt(), $W.NoneStr(), $W.WNil()))

fn keep {n:nat} (l: $W.widget_list(n)): $W.widget_list(n + 1) = $W._wlist_remove_by_id(l, $W.Root())
