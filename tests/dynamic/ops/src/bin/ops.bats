#include "share/atspre_staload.hats"
#use array as A
#use arith as AR
#use str as S
#use widget as W

(* Each public widget operation, on the widget it returns and the diff it
   reports: children appended in order, removed by generated id, cleared;
   hidden, class, tabindex and title set on the element; inject_css's two
   diffs; a Text widget left as it was; a tree of every element kind that
   carries values, copied and freed. One line per check.

   Every widget, diff and value made here is let go of, and every text is
   a literal (a text built from bytes is never freed): the test runs
   under valgrind (the `no-leaks` file), which finds no block lost. *)

fun len {n,s:nat} .<n>. (l: !$W.widget_list(n, s)): int n =
  case+ l of
  | $W.WNil() => 0
  | $W.WCons(_, rest) => 1 + len(rest)

fn gid {n:pos | n < 256} (t: string n): $W.widget_id =
  $W.Generated($A.text_lit(t), g1u2i(string1_length(t)))

fn div (id: $W.widget_id): $W.widget =
  $W.Element($W.ElementNode(id, $W.Normal($W.Div()), $W.NoClass(), false, $W.NoneInt(), $W.NoneStr(), $W.WNil()))

fn id_is (a: $W.widget_id, b: $W.widget_id): bool = $W._widget_id_eq(a, b)

(* The number of children of an element, or ~1 for text *)
fn nkids {s:pos} (w: !$W.widget_sz(s)): int =
  case+ w of
  | $W.Element($W.ElementNode(_, _, _, _, _, _, ch)) => len(ch)
  | $W.Text(_, _) => ~1

(* Whether the i-th child (from 0) of w has id *)
fun kid_at {n,s:nat} .<n>. (l: !$W.widget_list(n, s), i: int, id: $W.widget_id): bool =
  case+ l of
  | $W.WNil() => false
  | $W.WCons(c, rest) =>
    if i > 0 then kid_at(rest, i - 1, id)
    else (case+ c of
          | $W.Element($W.ElementNode(cid, _, _, _, _, _, _)) => id_is(cid, id)
          | $W.Text(_, _) => false)

(* Whether w is an element with id *)
fn is_id {s:pos} (w: !$W.widget_sz(s), id: $W.widget_id): bool =
  case+ w of
  | $W.Element($W.ElementNode(cid, _, _, _, _, _, _)) => id_is(cid, id)
  | $W.Text(_, _) => false

fn child_is {s:pos} (w: !$W.widget_sz(s), i: int, id: $W.widget_id): bool =
  case+ w of
  | $W.Element($W.ElementNode(_, _, _, _, _, _, ch)) => kid_at(ch, i, id)
  | $W.Text(_, _) => false

fn report (name: string, ok: bool): void =
  if ok then println! ("ok   ", name) else println! ("FAIL ", name)

(* The number of nodes in a tree *)
fun count {s:pos} .<s, 0>. (w: !$W.widget_sz(s)): int s =
  case+ w of
  | $W.Text(_, _) => 1
  | $W.Element($W.ElementNode(_, _, _, _, _, _, kids)) => 1 + count_list(kids)
and count_list {k,s:nat} .<s, 1>. (l: !$W.widget_list(k, s)): int s =
  case+ l of
  | $W.WNil() => 0
  | $W.WCons(w, rest) => count(w) + count_list(rest)

fn el {n:pos | n < 256} (t: string n, top: $W.html_top): $W.widget =
  $W.Element($W.ElementNode(gid(t), top, $W.NoClass(), false, $W.NoneInt(), $W.NoneStr(), $W.WNil()))

(* Appends child to w, letting go of the diff *)
fn put (w: $W.widget, child: $W.widget): $W.widget = let
  val @(w2, d) = $W.add_child(w, child)
  val () = $W.diff_free(d)
in w2 end

(* A form holding one element of each kind that carries values (and
   the tree it makes when built: 13 nodes) *)
fn rich (): $W.widget = let
  val f = el("f1", $W.Normal($W.Form($A.text_lit("/go"), 3, $W.FormPost(), $W.EnctypePlain())))
  val f = put(f, el("a1", $W.Normal($W.A($A.text_lit("/x"), 2, $W.TargetIs($W.NamedTarget($A.text_lit("win"), 3))))))
  val f = put(f, el("a2", $W.Normal($W.A($A.text_lit("/y"), 2, $W.TargetIs($W.Blank())))))
  val f = put(f, el("o1", $W.Normal($W.Ol($W.OlTypeIs($W.OlUpperRoman())))))
  val f = put(f, el("l1", $W.Normal($W.Label($W.SomeStr($A.text_lit("i1"), 2)))))
  val f = put(f, el("t1", $W.Normal($W.Th(2, 1, $W.ScopeIs($W.ScopeCol())))))
  val f = put(f, el("i1", $W.Void($W.HtmlInput($W.InputText(), $W.SomeStr($A.text_lit("q"), 1),
    $W.SomeStr($A.text_lit("v"), 1), false, false, true))))
  val f = put(f, el("k1", $W.Void($W.Track($A.text_lit("s.vtt"), 5, $W.TrackCaptions(), $W.SomeStr($A.text_lit("en"), 2)))))
  val f = put(f, el("m1", $W.Void($W.Img($A.text_lit("p.png"), 5, $A.text_lit("a"), 1, $W.LoadingLazy()))))
  val f = put(f, el("b1", $W.Normal($W.Button($W.ButtonSubmit()))))
  val b = put(el("s1", $W.Normal($W.Span())), $W.Text($A.text_lit("hi"), 2))
  val f = put(f, b)
  val f = put(f, $W.Text($A.text_lit("end"), 3))
  val @(f, d1) = $W.set_class(f, 4)
  val () = $W.diff_free(d1)
  val @(f, d2) = $W.set_tabindex(f, $W.SomeInt(1))
  val () = $W.diff_free(d2)
  val @(f, d3) = $W.set_title(f, $W.SomeStr($A.text_lit("form"), 4))
  val () = $W.diff_free(d3)
in f end

implement main0 () = let
  val root = div($W.Root())
  (* ids are compared by text *)
  val () = report("id_eq_generated", id_is(gid("b1"), gid("b1")))
  val () = report("id_ne_generated", ~id_is(gid("b1"), gid("b2")))
  val () = report("id_ne_root", ~id_is($W.Root(), gid("b1")))

  (* add_child appends, in order, and reports the parent and a copy of
     the child *)
  val @(w1, d1) = $W.add_child(root, div(gid("b1")))
  val k1 = nkids(w1)
  val c10 = child_is(w1, 0, gid("b1"))
  val () = report("add_one", k1 = 1 && c10)
  val () = report("add_diff", (case+ d1 of
    | $W.AddChild(p, c) => id_is(p, $W.Root()) && is_id(c, gid("b1")) | _ => false))
  val () = $W.diff_free(d1)
  val @(w2, dx1) = $W.add_child(w1, div(gid("b2")))
  val () = $W.diff_free(dx1)
  val @(w3, dx2) = $W.add_child(w2, div(gid("b3")))
  val () = $W.diff_free(dx2)
  (* each read in a val of its own: a linear widget read in only one
     branch of && does not type-check *)
  val k3 = nkids(w3)
  val c30 = child_is(w3, 0, gid("b1"))
  val c31 = child_is(w3, 1, gid("b2"))
  val c32 = child_is(w3, 2, gid("b3"))
  val () = report("add_order", k3 = 3 && c30 && c31 && c32)

  (* remove_child removes the child with that id, and only it *)
  val @(w4, d4) = $W.remove_child(w3, gid("b2"))
  val k4 = nkids(w4)
  val c40 = child_is(w4, 0, gid("b1"))
  val c41 = child_is(w4, 1, gid("b3"))
  val () = report("remove_middle", k4 = 2 && c40 && c41)
  val () = report("remove_diff", (case+ d4 of
    | $W.RemoveChild(p, c) => id_is(p, $W.Root()) && id_is(c, gid("b2")) | _ => false))
  val () = $W.diff_free(d4)
  val @(w5, dx3) = $W.remove_child(w4, gid("zz"))
  val () = $W.diff_free(dx3)
  val () = report("remove_unknown", nkids(w5) = 2)

  (* remove_all_children empties the list *)
  val @(w6, d6) = $W.remove_all_children(w5)
  val () = report("remove_all", nkids(w6) = 0)
  val () = report("remove_all_diff", (case+ d6 of
    | $W.RemoveAllChildren(p) => id_is(p, $W.Root()) | _ => false))
  val () = $W.diff_free(d6)
  val @(w7, dx4) = $W.add_child(w6, div(gid("b4")))
  val () = $W.diff_free(dx4)
  val k7 = nkids(w7)
  val c70 = child_is(w7, 0, gid("b4"))
  val () = report("add_after_clear", k7 = 1 && c70)

  (* set_hidden, set_class, set_tabindex, set_title update the element *)
  val el0 = div(gid("e1"))
  val @(h, dh) = $W.set_hidden(el0, true)
  val () = report("hidden", (case+ h of
    | $W.Element($W.ElementNode(_, _, _, hv, _, _, _)) => hv | _ => false))
  val () = report("hidden_diff", (case+ dh of
    | $W.SetHidden(t, v) => id_is(t, gid("e1")) && v | _ => false))
  val () = $W.diff_free(dh)
  val @(c, dc) = $W.set_class(h, 675)
  val () = report("class", (case+ c of
    | $W.Element($W.ElementNode(_, _, $W.ClassIdx(i), _, _, _, _)) => i = 675 | _ => false))
  val () = report("class_diff", (case+ dc of
    | $W.SetClass(t, i, txt, n) =>
        if n = 3 then id_is(t, gid("e1")) && i = 675 &&
          byte2int0($A.text_get(txt, 0)) = 99 && byte2int0($A.text_get(txt, 1)) = 122 &&
          byte2int0($A.text_get(txt, 2)) = 122
        else false
    | _ => false))
  val () = $W.diff_free(dc)
  val @(t, dt) = $W.set_tabindex(c, $W.SomeInt(3))
  val () = report("tabindex", (case+ t of
    | $W.Element($W.ElementNode(_, _, _, _, $W.SomeInt(v), _, _)) => v = 3 | _ => false))
  val () = report("tabindex_diff", (case+ dt of
    | $W.SetTabindex(tg, $W.SomeInt(v)) => id_is(tg, gid("e1")) && v = 3 | _ => false))
  val () = $W.diff_free(dt)
  val @(ti, dti) = $W.set_title(t, $W.SomeStr($A.text_lit("tip"), 3))
  val () = report("title", (case+ ti of
    | $W.Element($W.ElementNode(_, _, _, _, _, $W.SomeStr(_, n), _)) => n = 3 | _ => false))
  val () = report("title_diff", (case+ dti of
    | $W.SetTitle(tg, $W.SomeStr(_, n)) => id_is(tg, gid("e1")) && n = 3 | _ => false))
  val () = $W.diff_free(dti)
  val @(ti, dti2) = $W.set_title(ti, $W.NoneStr())
  val () = report("title_unset_diff", (case+ dti2 of
    | $W.SetTitle(tg, $W.NoneStr()) => id_is(tg, gid("e1")) | _ => false))
  val () = $W.diff_free(dti2)
  val () = report("all_kept", (case+ ti of
    | $W.Element($W.ElementNode(_, _, $W.ClassIdx(i), hv, $W.SomeInt(v), $W.NoneStr(), _)) =>
        i = 675 && hv && v = 3
    | _ => false))
  val () = $W.widget_free(ti)

  (* inject_css: the style element is appended, then its text is set *)
  val @(wc, dl) = $W.inject_css(w7, gid("s1"), $A.text_lit("p q"), 3)
  val kc = nkids(wc)
  val cc1 = child_is(wc, 1, gid("s1"))
  val () = report("css_child", kc = 2 && cc1)
  val () = report("css_diffs", (case+ dl of
    | $W.DLCons($W.AddChild(p, _), $W.DLCons($W.SetTextContent(s, _, n), $W.DLNil())) =>
        id_is(p, $W.Root()) && id_is(s, gid("s1")) && n = 3
    | _ => false))
  val () = $W.diff_list_free(dl)
  val () = $W.widget_free(wc)

  (* A Text widget is left as it was *)
  val tw = $W.Text($A.text_lit("hi"), 2)
  val @(tw2, dtw) = $W.set_hidden(tw, true)
  val () = report("text_unchanged", nkids(tw2) = ~1)
  val () = report("text_diff_root", (case+ dtw of
    | $W.SetHidden(tg, _) => id_is(tg, $W.Root()) | _ => false))
  val () = $W.diff_free(dtw)
  val () = $W.widget_free(tw2)

  (* A tree of every kind that carries values: a copy is its own, and
     both are freed with everything in them *)
  val r = rich()
  val rc = $W.widget_copy(r)
  val nr = count(r)
  val nrc = count(rc)
  val () = report("rich_size", nr = 13 && nrc = 13)
  val @(rc, dr) = $W.remove_child(rc, gid("a1"))
  val () = $W.diff_free(dr)
  val nr = count(r)
  val nrc = count(rc)
  val () = report("copy_own", nr = 13 && nrc = 12)
  val () = $W.widget_free(rc)
  val @(r, dr2) = $W.remove_all_children(r)
  val () = $W.diff_free(dr2)
  val () = report("rich_cleared", count(r) = 1)
  val () = $W.widget_free(r)

  (* Diffs that carry values, let go of without being applied *)
  val () = $W.diff_free($W.SetAttribute(gid("a1"), $W.SetATarget($W.TargetIs($W.NamedTarget($A.text_lit("w"), 1)))))
  val () = $W.diff_free($W.SetAttribute(gid("t1"), $W.SetThScope($W.ScopeIs($W.ScopeRow()))))
  val () = $W.diff_free($W.SetAttribute(gid("i1"), $W.SetInputName($W.SomeStr($A.text_lit("n"), 1))))
  val () = $W.diff_free($W.SetAttribute(gid("i1"), $W.SetInputValue($W.SomeStr($A.text_lit("v"), 1))))
  val () = $W.diff_free($W.AddChild($W.Root(), rich()))
  val () = report("diffs_freed", true)
in () end
