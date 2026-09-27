#include "share/atspre_staload.hats"
#use array as A
#use str as S
#use widget as W

fn div (): $W.widget =
  $W.Element($W.ElementNode($W.Root(), $W.Normal($W.Div()), $W.NoClass(), false, $W.NoneInt(), $W.NoneStr(), $W.WNil()))

fn two (): $W.widget_list(2) = $W._wlist_append($W.WNil(), div())
