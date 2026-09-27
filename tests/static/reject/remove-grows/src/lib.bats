#include "share/atspre_staload.hats"
#use array as A
#use str as S
#use widget as W

fn div (): $W.widget =
  $W.Element($W.ElementNode($W.Root(), $W.Normal($W.Div()), $W.NoClass(), false, $W.NoneInt(), $W.NoneStr(), $W.WNil()))

fn keep {n,s:nat} (l: $W.widget_list(n, s)): [s2:nat] $W.widget_list(n + 1, s2) = $W._wlist_remove_by_id(l, $W.Root())
