#include "share/atspre_staload.hats"
#use widget as W

(* A cell spans at least one column *)
fn cell (): $W.html_normal = $W.Td(0, 1)
