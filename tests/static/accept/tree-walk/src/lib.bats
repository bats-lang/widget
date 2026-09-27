#include "share/atspre_staload.hats"
#use widget as W

(* The number of nodes in a widget tree, which is its size: the walk
   terminates on the size index, with no fuel *)
#pub fn count {s:pos} (w: $W.widget_sz(s)): int s

implement count (w) = let
  fun node {s:pos} .<s, 0>. (w: $W.widget_sz(s)): int s =
    case+ w of
    | $W.Text(_, _) => 1
    | $W.Element($W.ElementNode(_, _, _, _, _, _, kids)) => 1 + list(kids)
  and list {k,s:nat} .<s, 1>. (l: $W.widget_list(k, s)): int s =
    case+ l of
    | $W.WNil() => 0
    | $W.WCons(w, rest) => node(w) + list(rest)
in node(w) end
