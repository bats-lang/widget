#include "share/atspre_staload.hats"
#use array as A
#use str as S
#use widget as W

(* A div holding a text node has two nodes; claiming size 1 does not
   type-check *)
fn one (): $W.widget_sz(1) = let
  var c = @[char][1]('x')
in
  $W.Element($W.ElementNode($W.Root(), $W.Normal($W.Div()), $W.NoClass(), false,
    $W.NoneInt(), $W.NoneStr(), $W.WCons($W.Text($S.text_of_chars(c, 1), 1), $W.WNil())))
end
