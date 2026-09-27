#include "share/atspre_staload.hats"
#use array as A
#use arith as AR
#use str as S
#use widget as W

(* Each public widget operation, on the widget it returns and the diff it
   reports: children appended in order, removed by generated id, cleared;
   hidden, class, tabindex and title set on the element; inject_css's two
   diffs; a Text widget left as it was. One line per check. *)

fun len {n,s:nat} .<n>. (l: $W.widget_list(n, s)): int n =
  case+ l of
  | $W.WNil() => 0
  | $W.WCons(_, rest) => 1 + len(rest)

fn gid (a: char, b: char): $W.widget_id = let
  var c = @[char][2](a, b)
in $W.Generated($S.text_of_chars(c, 2), 2) end

fn div (id: $W.widget_id): $W.widget =
  $W.Element($W.ElementNode(id, $W.Normal($W.Div()), $W.NoClass(), false, $W.NoneInt(), $W.NoneStr(), $W.WNil()))

fn id_is (a: $W.widget_id, b: $W.widget_id): bool = $W._widget_id_eq(a, b)

(* The number of children of an element, or ~1 for text *)
fn nkids (w: $W.widget): int =
  case+ w of
  | $W.Element($W.ElementNode(_, _, _, _, _, _, ch)) => len(ch)
  | $W.Text(_, _) => ~1

(* Whether the i-th child (from 0) of w has id *)
fun kid_at {n,s:nat} .<n>. (l: $W.widget_list(n, s), i: int, id: $W.widget_id): bool =
  case+ l of
  | $W.WNil() => false
  | $W.WCons(c, rest) =>
    if i > 0 then kid_at(rest, i - 1, id)
    else (case+ c of
          | $W.Element($W.ElementNode(cid, _, _, _, _, _, _)) => id_is(cid, id)
          | $W.Text(_, _) => false)

fn child_is (w: $W.widget, i: int, id: $W.widget_id): bool =
  case+ w of
  | $W.Element($W.ElementNode(_, _, _, _, _, _, ch)) => kid_at(ch, i, id)
  | $W.Text(_, _) => false

fn report (name: string, ok: bool): void =
  if ok then println! ("ok   ", name) else println! ("FAIL ", name)

implement main0 () = let
  val root = div($W.Root())
  (* ids are compared by text *)
  val () = report("id_eq_generated", id_is(gid('b', '1'), gid('b', '1')))
  val () = report("id_ne_generated", ~id_is(gid('b', '1'), gid('b', '2')))
  val () = report("id_ne_root", ~id_is($W.Root(), gid('b', '1')))

  (* add_child appends, in order, and reports the parent *)
  val @(w1, d1) = $W.add_child(root, div(gid('b', '1')))
  val () = report("add_one", nkids(w1) = 1 && child_is(w1, 0, gid('b', '1')))
  val () = report("add_diff", (case+ d1 of
    | $W.AddChild(p, _) => id_is(p, $W.Root()) | _ => false))
  val () = $W.diff_free(d1)
  val @(w2, dx1) = $W.add_child(w1, div(gid('b', '2')))
  val () = $W.diff_free(dx1)
  val @(w3, dx2) = $W.add_child(w2, div(gid('b', '3')))
  val () = $W.diff_free(dx2)
  val () = report("add_order", nkids(w3) = 3 && child_is(w3, 0, gid('b', '1')) &&
    child_is(w3, 1, gid('b', '2')) && child_is(w3, 2, gid('b', '3')))

  (* remove_child removes the child with that id, and only it *)
  val @(w4, d4) = $W.remove_child(w3, gid('b', '2'))
  val () = report("remove_middle", nkids(w4) = 2 && child_is(w4, 0, gid('b', '1')) &&
    child_is(w4, 1, gid('b', '3')))
  val () = report("remove_diff", (case+ d4 of
    | $W.RemoveChild(p, c) => id_is(p, $W.Root()) && id_is(c, gid('b', '2')) | _ => false))
  val () = $W.diff_free(d4)
  val @(w5, dx3) = $W.remove_child(w4, gid('z', 'z'))
  val () = $W.diff_free(dx3)
  val () = report("remove_unknown", nkids(w5) = 2)

  (* remove_all_children empties the list *)
  val @(w6, d6) = $W.remove_all_children(w5)
  val () = report("remove_all", nkids(w6) = 0)
  val () = report("remove_all_diff", (case+ d6 of
    | $W.RemoveAllChildren(p) => id_is(p, $W.Root()) | _ => false))
  val () = $W.diff_free(d6)
  val @(w7, dx4) = $W.add_child(w6, div(gid('b', '4')))
  val () = $W.diff_free(dx4)
  val () = report("add_after_clear", nkids(w7) = 1 && child_is(w7, 0, gid('b', '4')))

  (* set_hidden, set_class, set_tabindex, set_title update the element *)
  val el = div(gid('e', '1'))
  val @(h, dh) = $W.set_hidden(el, true)
  val () = report("hidden", (case+ h of
    | $W.Element($W.ElementNode(_, _, _, hv, _, _, _)) => hv | _ => false))
  val () = report("hidden_diff", (case+ dh of
    | $W.SetHidden(t, v) => id_is(t, gid('e', '1')) && v | _ => false))
  val () = $W.diff_free(dh)
  val @(c, dc) = $W.set_class(el, 675)
  val () = report("class", (case+ c of
    | $W.Element($W.ElementNode(_, _, $W.ClassIdx(i), _, _, _, _)) => i = 675 | _ => false))
  val () = report("class_diff", (case+ dc of
    | $W.SetClass(t, i, txt, n) =>
        if n = 3 then id_is(t, gid('e', '1')) && i = 675 &&
          byte2int0($A.text_get(txt, 0)) = 99 && byte2int0($A.text_get(txt, 1)) = 122 &&
          byte2int0($A.text_get(txt, 2)) = 122
        else false
    | _ => false))
  val () = $W.diff_free(dc)
  val @(t, dt) = $W.set_tabindex(el, $W.SomeInt(3))
  val () = report("tabindex", (case+ t of
    | $W.Element($W.ElementNode(_, _, _, _, $W.SomeInt(v), _, _)) => v = 3 | _ => false))
  val () = report("tabindex_diff", (case+ dt of
    | $W.SetTabindex(tg, $W.SomeInt(v)) => id_is(tg, gid('e', '1')) && v = 3 | _ => false))
  val () = $W.diff_free(dt)
  val @(ti, dti) = $W.set_title(el, $W.NoneStr())
  val () = report("title_diff", (case+ dti of
    | $W.SetTitle(tg, $W.NoneStr()) => id_is(tg, gid('e', '1')) | _ => false))
  val () = $W.diff_free(dti)

  (* inject_css: the style element is appended, then its text is set *)
  var css = @[char][3]('p', ' ', 'q')
  val @(wc, dl) = $W.inject_css(root, gid('s', '1'), $S.text_of_chars(css, 3), 3)
  val () = report("css_child", nkids(wc) = 1 && child_is(wc, 0, gid('s', '1')))
  val () = report("css_diffs", (case+ dl of
    | $W.DLCons($W.AddChild(p, _), $W.DLCons($W.SetTextContent(s, _, n), $W.DLNil())) =>
        id_is(p, $W.Root()) && id_is(s, gid('s', '1')) && n = 3
    | _ => false))
  val () = $W.diff_list_free(dl)

  (* A Text widget is left as it was *)
  var tc = @[char][2]('h', 'i')
  val tw = $W.Text($S.text_of_chars(tc, 2), 2)
  val @(tw2, dtw) = $W.set_hidden(tw, true)
  val () = report("text_unchanged", nkids(tw2) = ~1)
  val () = report("text_diff_root", (case+ dtw of
    | $W.SetHidden(tg, _) => id_is(tg, $W.Root()) | _ => false))
  val () = $W.diff_free(dtw)
in () end
