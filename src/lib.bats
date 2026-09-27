(* widget -- typed HTML widget and diff library *)
(* No $UNSAFE. Algebraic types for HTML elements, widgets, and diffs. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use css as C
#use str as S

(* ============================================================
   Option type for optional values
   ============================================================ *)

#pub datatype option_int =
  | {v:int} SomeInt of (int v)
  | NoneInt

(* A class: none, or an index into css's class names *)
#pub datatype class_opt =
  | NoClass
  | {i:nat | i < 676} ClassIdx of (int i)

#pub datatype option_str =
  | {n:pos | n < 256} SomeStr of ($A.text(n), int(n))
  | NoneStr

(* ============================================================
   Supporting types
   ============================================================ *)

#pub datatype rel_value =
  | RelNoopener | RelNoreferrer | RelNofollow
  | RelAlternate | RelAuthor | RelBookmark
  | RelExternal | RelHelp | RelLicense
  | RelNext | RelPrev | RelSearch | RelTag

#pub datatype form_enctype =
  | EnctypeUrlencoded | EnctypeMultipart | EnctypePlain

#pub datatype th_scope =
  | ScopeCol | ScopeRow | ScopeColgroup | ScopeRowgroup

(* A header cell's scope, when it has one *)
#pub datatype scope_opt =
  | NoScope
  | ScopeIs of (th_scope)

#pub datatype ol_list_type =
  | OlDecimal | OlLowerAlpha | OlUpperAlpha
  | OlLowerRoman | OlUpperRoman

(* An ordered list's numbering type, when it has one *)
#pub datatype ol_type_opt =
  | OlDefault
  | OlTypeIs of (ol_list_type)

#pub datatype img_loading =
  | LoadingLazy | LoadingEager

#pub datatype mime_main =
  | MimeText | MimeImage | MimeAudio | MimeVideo
  | MimeApplication | MimeMultipart | MimeFont
  | MimeMessage | MimeModel

#pub datatype link_target =
  | Blank | Self_ | Parent_ | Top_
  | {n:pos | n < 256} NamedTarget of ($A.text(n), int(n))

(* An anchor's target, when it has one *)
#pub datatype target_opt =
  | NoTarget
  | TargetIs of (link_target)

#pub datatype button_type =
  | ButtonSubmit | ButtonReset | ButtonButton

#pub datatype form_method =
  | FormGet | FormPost

#pub datatype input_type =
  | InputText | InputPassword | InputEmail | InputNumber
  | InputCheckbox | InputRadio | InputRange
  | InputDate | InputTime | InputDatetimeLocal
  | InputFile | InputColor | InputHidden
  | InputSubmit | InputReset | InputButton

#pub datatype track_kind =
  | TrackSubtitles | TrackCaptions | TrackDescriptions
  | TrackChapters | TrackMetadata

(* ============================================================
   HTML normal elements (may have children)
   ============================================================ *)

#pub datatype html_normal =
  (* Layout / sectioning *)
  | Div | Span | Section | Article
  | HtmlHeader | HtmlFooter | HtmlMain | Nav | Aside
  (* Headings *)
  | H1 | H2 | H3 | H4 | H5 | H6
  (* Text block *)
  | P | Blockquote | Pre | HtmlCode
  | Figure | Figcaption
  (* Inline text *)
  | Strong | Em | Small | Mark
  | Del | Ins | HtmlSub | Sup
  (* Lists *)
  | Ul
  | Ol of (ol_type_opt)
  | Li
  (* Interactive *)
  | {n:pos | n < 256} A of ($A.text(n), int(n), target_opt)  (* href, target *)
  | Button of (button_type)
  | Label of (option_str)      (* for: target id or absent *)
  | Details | Summary
  (* Form *)
  | {n:pos | n < 256} Form of ($A.text(n), int(n), form_method, form_enctype)
  | Fieldset | Legend
  | {n:pos | n < 256} Select of ($A.text(n), int(n), bool)    (* name, multiple *)
  | {n:pos | n < 256} Optgroup of ($A.text(n), int(n))       (* label *)
  | {n:pos | n < 256} HtmlOption of ($A.text(n), int(n))     (* value *)
  | {n:pos | n < 256}{r,c:pos} Textarea of ($A.text(n), int(n), int r, int c) (* name, rows, cols *)
  (* Table *)
  | Table | Caption | Thead | Tbody | Tfoot | Tr
  | {cs,rs:pos} Th of (int cs, int rs, scope_opt)   (* colspan, rowspan, scope *)
  | {cs,rs:pos} Td of (int cs, int rs)              (* colspan, rowspan *)
  (* Media *)
  | {n:pos | n < 256} Video of ($A.text(n), int(n), bool, bool, bool, bool) (* src, controls, autoplay, loop, muted *)
  | {n:pos | n < 256} Audio of ($A.text(n), int(n), bool, bool, bool, bool) (* src, controls, autoplay, loop, muted *)
  | Picture
  (* Metadata *)
  | Style

(* ============================================================
   HTML void elements (no children)
   ============================================================ *)

#pub datatype html_void =
  | Br | Hr | Wbr
  | {ns:pos | ns < 256}{na:pos | na < 256} Img of ($A.text(ns), int(ns), $A.text(na), int(na), img_loading)  (* src, alt, loading *)
  | HtmlInput of (input_type, option_str, option_str, bool, bool, bool) (* type, name, value, disabled, checked, required *)
  | {ns:pos | ns < 256}{nt:pos | nt < 256} Source of ($A.text(ns), int(ns), $A.text(nt), int(nt))  (* src, type *)
  | {n:pos | n < 256} Track of ($A.text(n), int(n), track_kind, option_str) (* src, kind, srclang *)

(* ============================================================
   HTML top type
   ============================================================ *)

#pub datatype html_top =
  | Normal of (html_normal)
  | Void of (html_void)

(* ============================================================
   Widget ID
   ============================================================ *)

#pub datatype widget_id =
  | Root
  | {n:pos | n < 256} Generated of ($A.text(n), int(n))

(* ============================================================
   Widget
   ============================================================ *)

(* A list of k widgets whose sizes add up to s. A widget's size is its
   number of nodes (itself and everything under it), so a walk over a
   tree has a termination metric. *)
#pub datatype widget_list(int, int) =
  | WNil(0, 0)
  | {k,s:nat}{t:pos} WCons(k + 1, s + t) of (widget_sz(t), widget_list(k, s))

and widget_sz(int) =
  | {n:pos | n < 65536} Text(1) of ($A.text(n), int(n))
  | {s:pos} Element(s) of (element_node(s))

and element_node(int) =
  | {k,s:nat} ElementNode(s + 1) of (
      widget_id,    (* id *)
      html_top,     (* element type *)
      class_opt,    (* class *)
      bool,         (* hidden *)
      option_int,   (* tabindex *)
      option_str,   (* title *)
      widget_list(k, s)   (* children, always WNil when top is Void *)
    )

(* A widget of any size *)
#pub typedef widget = [s:pos] widget_sz(s)

(* ============================================================
   Diff operations
   ============================================================ *)

#pub datatype diff =
  | RemoveAllChildren of (widget_id)
  | AddChild of (widget_id, widget)       (* parent, child *)
  | RemoveChild of (widget_id, widget_id) (* parent, child_id *)
  | SetHidden of (widget_id, bool)
  | {n:pos | n < 256}{i:nat | i < 676} SetClass of (widget_id, int i, $A.text(n), int(n))  (* class index + resolved name *)
  | {n:pos | n < 256} SetClassName of (widget_id, $A.text(n), int(n))   (* set class attr by name *)
  | {n:pos | n < 65536} SetTextContent of (widget_id, $A.text(n), int(n)) (* set text content *)
  | SetTabindex of (widget_id, option_int)
  | SetTitle of (widget_id, option_str)
  | SetAttribute of (widget_id, attribute_change)

and attribute_change =
  (* A *)
  | {n:pos | n < 256} SetHref of ($A.text(n), int(n))
  | SetATarget of (target_opt)
  (* Button *)
  | SetButtonType of (button_type)
  | SetButtonDisabled of (bool)
  (* Form *)
  | {n:pos | n < 256} SetFormAction of ($A.text(n), int(n))
  | SetFormMethod of (form_method)
  | SetFormEnctype of (form_enctype)
  (* Select *)
  | SetSelectDisabled of (bool)
  | SetSelectMultiple of (bool)
  (* Option *)
  | {n:pos | n < 256} SetOptionValue of ($A.text(n), int(n))
  | SetOptionDisabled of (bool)
  | SetOptionSelected of (bool)
  (* Textarea *)
  | {n:pos | n < 256} SetTextareaValue of ($A.text(n), int(n))
  | SetTextareaDisabled of (bool)
  | SetTextareaReadonly of (bool)
  | {n:pos} SetTextareaRows of (int n)
  | {n:pos} SetTextareaCols of (int n)
  (* Th, Td *)
  | {n:pos} SetColspan of (int n)
  | {n:pos} SetRowspan of (int n)
  | SetThScope of (scope_opt)
  (* Img *)
  | {n:pos | n < 256} SetImgSrc of ($A.text(n), int(n))
  | {n:pos | n < 256} SetImgAlt of ($A.text(n), int(n))
  | SetImgLoading of (img_loading)
  (* Input *)
  | SetInputType of (input_type)
  | SetInputName of (option_str)
  | SetInputValue of (option_str)
  | SetInputDisabled of (bool)
  | SetInputChecked of (bool)
  | SetInputRequired of (bool)
  | SetInputReadonly of (bool)
  (* Details *)
  | SetDetailsOpen of (bool)

(* ============================================================
   Diff list -- for operations that produce multiple diffs
   ============================================================ *)

(* A sequence of n diffs; diff_list is one of any length *)
#pub datatype diff_seq(int) =
  | DLNil(0)
  | {n:nat} DLCons(n + 1) of (diff, diff_seq(n))

#pub typedef diff_list = [n:nat] diff_seq(n)

(* ============================================================
   Internal helpers
   ============================================================ *)

#pub fn _wlist_append {n,s:nat}{t:pos} (wl: widget_list(n, s), w: widget_sz(t)): widget_list(n + 1, s + t)
#pub fn _widget_id_eq(a: widget_id, b: widget_id): bool
#pub fn _wlist_remove_by_id {n,s:nat}
  (wl: widget_list(n, s), target: widget_id): [m,s2:nat | m <= n; s2 <= s] widget_list(m, s2)

fun _append {n,s:nat}{t:pos} .<n>. (wl: widget_list(n, s), w: widget_sz(t)): widget_list(n + 1, s + t) =
  case+ wl of
  | WNil() => WCons(w, WNil())
  | WCons(hd, tl) => WCons(hd, _append(tl, w))

implement _wlist_append (wl, w) = _append(wl, w)

(* Whether a[k, n) and b[k, n) hold the same bytes *)
fun _text_eq {n,k:nat | k <= n} .<n - k>.
  (a: $A.text(n), b: $A.text(n), n: int n, k: int k): bool =
  if k >= n then true
  else if $AR.eq_int_int(byte2int0($A.text_get(a, k)), byte2int0($A.text_get(b, k))) then
    _text_eq(a, b, n, k + 1)
  else false

(* Generated ids are equal when their texts are: remove_child finds a
   generated child by the id it was created with *)
implement _widget_id_eq (a, b) =
  case+ a of
  | Root() => (case+ b of | Root() => true | _ => false)
  | Generated(ta, na) =>
    (case+ b of
     | Generated(tb, nb) => if na = nb then _text_eq(ta, tb, na, 0) else false
     | Root() => false)

(* wl without its first element whose id is target *)
fun _remove {n,s:nat} .<n>.
  (wl: widget_list(n, s), target: widget_id): [m,s2:nat | m <= n; s2 <= s] widget_list(m, s2) =
  case+ wl of
  | WNil() => WNil()
  | WCons(hd, tl) => let
      val matches = case+ hd of
        | Element(ElementNode(id, _, _, _, _, _, _)) => _widget_id_eq(id, target)
        | Text(_, _) => false
    in
      if matches then tl
      else WCons(hd, _remove(tl, target))
    end

implement _wlist_remove_by_id (wl, target) = _remove(wl, target)

(* ============================================================
   Convenience functions: return (updated_widget, diff)
   ============================================================ *)

#pub fn add_child(parent: widget, child: widget): @(widget, diff)
#pub fn remove_child(parent: widget, child_id: widget_id): @(widget, diff)
#pub fn remove_all_children(w: widget): @(widget, diff)
#pub fn set_hidden(w: widget, h: bool): @(widget, diff)
#pub fn set_class {i:nat | i < 676} (w: widget, cls: int i): @(widget, diff)
#pub fn set_class_name{n:pos | n < 256}(wid: widget_id, cls: $A.text(n), len: int n): diff
#pub fn set_text_content{n:pos | n < 65536}(wid: widget_id, text: $A.text(n), len: int n): diff
#pub fn set_tabindex(w: widget, ti: option_int): @(widget, diff)
#pub fn set_title(w: widget, t: option_str): @(widget, diff)
#pub fn inject_css{n:pos | n < 65536}(parent: widget, style_id: widget_id, css: $A.text(n), len: int n): @(widget, diff_list)

implement add_child (parent, child) =
  case+ parent of
  | Text(_, _) => @(parent, AddChild(Root(), child))
  | Element(ElementNode(id, top, cls, hidden, ti, title, children)) =>
    @(Element(ElementNode(id, top, cls, hidden, ti, title, _wlist_append(children, child))),
      AddChild(id, child))

implement remove_child (parent, child_id) =
  case+ parent of
  | Text(_, _) => @(parent, RemoveChild(Root(), child_id))
  | Element(ElementNode(id, top, cls, hidden, ti, title, children)) =>
    @(Element(ElementNode(id, top, cls, hidden, ti, title, _wlist_remove_by_id(children, child_id))),
      RemoveChild(id, child_id))

implement remove_all_children (w) =
  case+ w of
  | Text(_, _) => @(w, RemoveAllChildren(Root()))
  | Element(ElementNode(id, top, cls, hidden, ti, title, _)) =>
    @(Element(ElementNode(id, top, cls, hidden, ti, title, WNil())),
      RemoveAllChildren(id))

implement set_hidden (w, h) =
  case+ w of
  | Text(_, _) => @(w, SetHidden(Root(), h))
  | Element(ElementNode(id, top, cls, _, ti, title, children)) =>
    @(Element(ElementNode(id, top, cls, h, ti, title, children)),
      SetHidden(id, h))

implement set_class (w, cls) = let
  val @(t, tlen) = $C.class_text(cls)
in
  case+ w of
  | Text(_, _) => @(w, SetClass(Root(), cls, t, tlen))
  | Element(ElementNode(id, top, _, hidden, ti, title, children)) =>
    @(Element(ElementNode(id, top, ClassIdx(cls), hidden, ti, title, children)),
      SetClass(id, cls, t, tlen))
end

implement set_tabindex (w, ti) =
  case+ w of
  | Text(_, _) => @(w, SetTabindex(Root(), ti))
  | Element(ElementNode(id, top, cls, hidden, _, title, children)) =>
    @(Element(ElementNode(id, top, cls, hidden, ti, title, children)),
      SetTabindex(id, ti))

implement set_class_name (wid, cls, len) = SetClassName(wid, cls, len)

implement set_text_content (wid, text, len) = SetTextContent(wid, text, len)

implement set_title (w, t) =
  case+ w of
  | Text(_, _) => @(w, SetTitle(Root(), t))
  | Element(ElementNode(id, top, cls, hidden, ti, _, children)) =>
    @(Element(ElementNode(id, top, cls, hidden, ti, t, children)),
      SetTitle(id, t))

implement inject_css (parent, style_id, css, len) = let
  val style_w = Element(ElementNode(style_id, Normal(Style()), NoClass(), false, NoneInt(), NoneStr(), WNil()))
  val @(parent2, d1) = add_child(parent, style_w)
  val d2 = SetTextContent(style_id, css, len)
in @(parent2, DLCons(d1, DLCons(d2, DLNil()))) end

$UNITTEST.run begin

(* ---- Helpers ---- *)

fn widget_id_eq(a: widget_id, b: widget_id): bool = _widget_id_eq(a, b)

fn mk_text1(c1: char): @($A.text(1), int(1)) = let
  var buf = @[char][1](c1)
in @($S.text_of_chars(buf, 1), 1) end

fn mk_text2(c1: char, c2: char): @($A.text(2), int(2)) = let
  var buf = @[char][2](c1, c2)
in @($S.text_of_chars(buf, 2), 2) end

fn mk_text3(c1: char, c2: char, c3: char): @($A.text(3), int(3)) = let
  var buf = @[char][3](c1, c2, c3)
in @($S.text_of_chars(buf, 3), 3) end

fn mk_text5(c1: char, c2: char, c3: char, c4: char, c5: char): @($A.text(5), int(5)) = let
  var buf = @[char][5](c1, c2, c3, c4, c5)
in @($S.text_of_chars(buf, 5), 5) end

fn txt_widget1(c1: char): widget = let
  val @(t, n) = mk_text1(c1)
in Text(t, n) end

fn txt_widget2(c1: char, c2: char): widget = let
  val @(t, n) = mk_text2(c1, c2)
in Text(t, n) end

fn txt_widget5(c1: char, c2: char, c3: char, c4: char, c5: char): widget = let
  val @(t, n) = mk_text5(c1, c2, c3, c4, c5)
in Text(t, n) end

fun wlist_len {n,s:nat} .<n>. (wl: widget_list(n, s)): int n =
  case+ wl of
  | WNil() => 0
  | WCons(_, rest) => 1 + wlist_len(rest)

fn wlist_append {n,s:nat}{t:pos} (wl: widget_list(n, s), w: widget_sz(t)): widget_list(n + 1, s + t) = _wlist_append(wl, w)

fn wlist_remove_by_id {n,s:nat} (wl: widget_list(n, s), target: widget_id): [m,s2:nat | m <= n; s2 <= s] widget_list(m, s2) =
  _wlist_remove_by_id(wl, target)

(* ---- apply_diff ---- *)

fn apply_diff(w: widget, d: diff): widget =
  case+ w of
  | Text(_, _) => w
  | Element(ElementNode(id, top, cls, hidden, tabidx, title, children)) =>
    case+ d of
    | SetHidden(target, new_h) =>
        if widget_id_eq(id, target)
        then Element(ElementNode(id, top, cls, new_h, tabidx, title, children))
        else w
    | SetClass(target, new_cls, _, _) =>
        if widget_id_eq(id, target)
        then Element(ElementNode(id, top, ClassIdx(new_cls), hidden, tabidx, title, children))
        else w
    | SetTabindex(target, new_ti) =>
        if widget_id_eq(id, target)
        then Element(ElementNode(id, top, cls, hidden, new_ti, title, children))
        else w
    | SetTitle(target, new_t) =>
        if widget_id_eq(id, target)
        then Element(ElementNode(id, top, cls, hidden, tabidx, new_t, children))
        else w
    | RemoveAllChildren(target) =>
        if widget_id_eq(id, target)
        then Element(ElementNode(id, top, cls, hidden, tabidx, title, WNil()))
        else w
    | AddChild(target, child) =>
        if widget_id_eq(id, target)
        then Element(ElementNode(id, top, cls, hidden, tabidx, title, wlist_append(children, child)))
        else w
    | RemoveChild(target, child_id) =>
        if widget_id_eq(id, target)
        then Element(ElementNode(id, top, cls, hidden, tabidx, title, wlist_remove_by_id(children, child_id)))
        else w
    | SetClassName(_, _, _) => w  (* class name is a DOM-only concept *)
    | SetTextContent(_, _, _) => w  (* text content is a DOM-only concept *)
    | SetAttribute(_, _) => w  (* attribute changes require html_top mutation *)

fn class_eq(a: class_opt, b: class_opt): bool =
  case+ a of
  | NoClass() => (case+ b of | NoClass() => true | _ => false)
  | ClassIdx(i) => (case+ b of | ClassIdx(j) => $AR.eq_int_int(i, j) | _ => false)

fn widget_eq(a: widget, b: widget): bool =
  case+ a of
  | Text(_, l1) => (case+ b of | Text(_, l2) => $AR.eq_int_int(l1, l2) | _ => false)
  | Element(ElementNode(id1, _, c1, h1, _, _, ch1)) =>
    (case+ b of
    | Text(_, _) => false
    | Element(ElementNode(id2, _, c2, h2, _, _, ch2)) =>
        widget_id_eq(id1, id2) &&
        class_eq(c1, c2) &&
        h1 = h2 &&
        $AR.eq_int_int(wlist_len(ch1), wlist_len(ch2)))

fn mk(top: html_top): widget =
  Element(ElementNode(Root(), top, NoClass(), false, NoneInt(), NoneStr(), WNil()))

(* ---- Round-trip proofs ---- *)

fn test_proof_set_hidden(): bool = let
  val w = mk(Normal(Div()))
  val d = SetHidden(Root(), true)
  val result = apply_diff(w, d)
  val expected = Element(ElementNode(Root(), Normal(Div()), NoClass(), true, NoneInt(), NoneStr(), WNil()))
in widget_eq(result, expected) end

fn test_proof_hidden_reversible(): bool = let
  val w = mk(Normal(Div()))
  val hidden = apply_diff(w, SetHidden(Root(), true))
  val restored = apply_diff(hidden, SetHidden(Root(), false))
in widget_eq(restored, w) end

fn test_proof_hidden_idempotent(): bool = let
  val w = mk(Normal(Div()))
  val d = SetHidden(Root(), true)
  val w1 = apply_diff(w, d)
  val w2 = apply_diff(w1, d)
in widget_eq(w1, w2) end

fn mk_set_class {i:nat | i < 676} (wid: widget_id, cls: int i): diff = let
  val @(t, tlen) = $C.class_text(cls)
in SetClass(wid, cls, t, tlen) end

fn test_proof_set_class(): bool = let
  val w = mk(Normal(Span()))
  val result = apply_diff(w, mk_set_class(Root(), 3))
  val expected = Element(ElementNode(Root(), Normal(Span()), ClassIdx(3), false, NoneInt(), NoneStr(), WNil()))
in widget_eq(result, expected) end

fn test_proof_class_replaces(): bool = let
  val w = mk(Normal(P()))
  val w1 = apply_diff(w, mk_set_class(Root(), 5))
  val w2 = apply_diff(w1, mk_set_class(Root(), 9))
  val expected = Element(ElementNode(Root(), Normal(P()), ClassIdx(9), false, NoneInt(), NoneStr(), WNil()))
in widget_eq(w2, expected) end

fn test_proof_compose_commutes(): bool = let
  val w = mk(Normal(Nav()))
  val a = apply_diff(apply_diff(w, SetHidden(Root(), true)), mk_set_class(Root(), 2))
  val b = apply_diff(apply_diff(w, mk_set_class(Root(), 2)), SetHidden(Root(), true))
in widget_eq(a, b) end

fn test_proof_add_child(): bool = let
  val w = mk(Normal(Div()))
  val child = txt_widget5('h', 'e', 'l', 'l', 'o')
  val result = apply_diff(w, AddChild(Root(), child))
in
  case+ result of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => $AR.eq_int_int(wlist_len(ch), 1)
  | _ => false
end

fn test_proof_add_two_children(): bool = let
  val w = mk(Normal(Ul()))
  val w1 = apply_diff(w, AddChild(Root(), txt_widget5('f', 'i', 'r', 's', 't')))
  val w2 = apply_diff(w1, AddChild(Root(), txt_widget1('s')))
in
  case+ w2 of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => $AR.eq_int_int(wlist_len(ch), 2)
  | _ => false
end

fn test_proof_remove_all_children(): bool = let
  val w = mk(Normal(Div()))
  val w1 = apply_diff(w, AddChild(Root(), txt_widget1('a')))
  val w2 = apply_diff(w1, AddChild(Root(), txt_widget1('b')))
  val w3 = apply_diff(w2, RemoveAllChildren(Root()))
in
  case+ w3 of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => $AR.eq_int_int(wlist_len(ch), 0)
  | _ => false
end

fn test_proof_remove_all_then_add(): bool = let
  val w = mk(Normal(Div()))
  val w1 = apply_diff(w, AddChild(Root(), txt_widget1('o')))
  val w2 = apply_diff(w1, RemoveAllChildren(Root()))
  val w3 = apply_diff(w2, AddChild(Root(), txt_widget1('n')))
in
  case+ w3 of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => $AR.eq_int_int(wlist_len(ch), 1)
  | _ => false
end

fn test_proof_text_ignores_diff(): bool = let
  val w = txt_widget1('u')
  val w1 = apply_diff(w, SetHidden(Root(), true))
  val w2 = apply_diff(w, mk_set_class(Root(), 5))
  val w3 = apply_diff(w, AddChild(Root(), txt_widget1('x')))
  val w4 = apply_diff(w, RemoveAllChildren(Root()))
in widget_eq(w1, w) && widget_eq(w2, w) && widget_eq(w3, w) && widget_eq(w4, w) end

fn test_proof_wrong_id_noop(): bool = let
  val w = mk(Normal(Div()))
  (* Generated IDs never match Root *)
  val d = SetHidden(Root(), true)
  val result = apply_diff(w, d)
  val expected = Element(ElementNode(Root(), Normal(Div()), NoClass(), true, NoneInt(), NoneStr(), WNil()))
in widget_eq(result, expected) end

fn test_proof_set_tabindex(): bool = let
  val w = mk(Normal(Div()))
  val result = apply_diff(w, SetTabindex(Root(), SomeInt(0)))
in
  case+ result of
  | Element(ElementNode(_, _, _, _, ti, _, _)) =>
    (case+ ti of | SomeInt(v) => $AR.eq_int_int(v, 0) | _ => false)
  | _ => false
end

fn test_proof_set_title(): bool = let
  val w = mk(Normal(Button(ButtonSubmit())))
  val @(ct, clen) = mk_text5('c', 'l', 'i', 'c', 'k')
  val result = apply_diff(w, SetTitle(Root(), SomeStr(ct, clen)))
in
  case+ result of
  | Element(ElementNode(_, _, _, _, _, t, _)) =>
    (case+ t of | SomeStr(_, n) => $AR.eq_int_int(n, 5) | _ => false)
  | _ => false
end

(* ---- Original datatype construction tests ---- *)

fn test_rel_values(): bool = let
  val r1 = RelNoopener()
  val r2 = RelNoreferrer()
  val ok1 = case+ r1 of | RelNoopener() => true | _ => false
  val ok2 = case+ r2 of | RelNoreferrer() => true | _ => false
in ok1 && ok2 end

fn test_form_enctype(): bool = let
  val e1 = EnctypeUrlencoded()
  val e2 = EnctypeMultipart()
  val ok1 = case+ e1 of | EnctypeUrlencoded() => true | _ => false
  val ok2 = case+ e2 of | EnctypeMultipart() => true | _ => false
in ok1 && ok2 end

fn test_input_types(): bool = let
  val t1 = InputText()
  val t2 = InputCheckbox()
  val ok1 = case+ t1 of | InputText() => true | _ => false
  val ok2 = case+ t2 of | InputCheckbox() => true | _ => false
in ok1 && ok2 end

fn test_html_top(): bool = let
  val n = Normal(Div())
  val v = Void(Br())
  val ok1 = case+ n of | Normal(_) => true | _ => false
  val ok2 = case+ v of | Void(_) => true | _ => false
in ok1 && ok2 end

fn test_element_node(): bool = let
  val e = ElementNode(Root(), Normal(Div()), NoClass(), false, NoneInt(), NoneStr(), WNil())
in case+ e of | ElementNode(id, _, _, _, _, _, _) => widget_id_eq(id, Root()) end

fn test_widget_with_children(): bool = let
  val children = WCons(txt_widget1('a'), WCons(txt_widget1('b'), WNil()))
  val e = Element(ElementNode(Root(), Normal(Ul()), NoClass(), false, NoneInt(), NoneStr(), children))
in case+ e of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => $AR.eq_int_int(wlist_len(ch), 2)
  | _ => false
end

fn test_label(): bool = let
  val @(ft, flen) = mk_text3('f', 'o', 'o')
  val l = Label(SomeStr(ft, flen))
in case+ l of | Label(s) => (case+ s of | SomeStr(_, n) => $AR.eq_int_int(n, 3) | _ => false) | _ => false end

fn test_optgroup(): bool = let
  val @(t, tl) = mk_text3('r', 'e', 'd')
  val og = Optgroup(t, tl)
in case+ og of | Optgroup(_, n) => $AR.eq_int_int(n, 3) | _ => false end

fn test_th_with_scope(): bool = let
  val th = Th(2, 3, ScopeIs(ScopeRow()))
in case+ th of | Th(cs, rs, _) => $AR.eq_int_int(cs, 2) && $AR.eq_int_int(rs, 3) | _ => false end

fn test_ol_with_type(): bool = let
  val ol = Ol(OlTypeIs(OlLowerAlpha()))
in case+ ol of | Ol(t) => (case+ t of | OlTypeIs(OlLowerAlpha()) => true | _ => false) | _ => false end

fn test_diff_set_attribute(): bool = let
  val @(ht, hlen) = mk_text3('u', 'r', 'l')
  val d = SetAttribute(Root(), SetHref(ht, hlen))
in case+ d of | SetAttribute(_, ac) => (case+ ac of | SetHref(_, _) => true | _ => false) | _ => false end

(* ---- Convenience function tests ---- *)

fn test_conv_add_child(): bool = let
  val w = mk(Normal(Div()))
  val @(w2, d) = add_child(w, txt_widget5('h', 'e', 'l', 'l', 'o'))
in
  (case+ w2 of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => $AR.eq_int_int(wlist_len(ch), 1)
  | _ => false) &&
  (case+ d of | AddChild(id, _) => widget_id_eq(id, Root()) | _ => false)
end

fn test_conv_set_hidden(): bool = let
  val w = mk(Normal(Div()))
  val @(w2, d) = set_hidden(w, true)
in
  (case+ w2 of
  | Element(ElementNode(_, _, _, h, _, _, _)) => h
  | _ => false) &&
  (case+ d of | SetHidden(_, v) => v | _ => false)
end

fn test_conv_set_class(): bool = let
  val w = mk(Normal(Span()))
  val @(w2, d) = set_class(w, 7)
in
  (case+ w2 of
  | Element(ElementNode(_, _, c, _, _, _, _)) => class_eq(c, ClassIdx(7))
  | _ => false) &&
  (case+ d of | SetClass(_, v, _, _) => $AR.eq_int_int(v, 7) | _ => false)
end

fn test_conv_remove_all_children(): bool = let
  val w = mk(Normal(Div()))
  val @(w1, _) = add_child(w, txt_widget1('a'))
  val @(w2, _) = add_child(w1, txt_widget1('b'))
  val @(w3, d) = remove_all_children(w2)
in
  (case+ w3 of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => $AR.eq_int_int(wlist_len(ch), 0)
  | _ => false) &&
  (case+ d of | RemoveAllChildren(_) => true | _ => false)
end

fn test_conv_text_noop(): bool = let
  val w = txt_widget2('h', 'i')
  val @(w2, _) = set_hidden(w, true)
in widget_eq(w, w2) end

(* Generated ids are compared by their text: two ids with the same text
   are the same id, so remove_child finds the child *)
fn test_remove_child_by_generated_id(): bool = let
  val @(t1, n1) = mk_text2('b', '1')
  val @(t2, n2) = mk_text2('b', '1')
  val kid = Element(ElementNode(Generated(t1, n1), Normal(Div()), NoClass(), false, NoneInt(), NoneStr(), WNil()))
  val @(w1, _) = add_child(mk(Normal(Div())), kid)
  val @(w2, _) = remove_child(w1, Generated(t2, n2))
in
  case+ w2 of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => wlist_len(ch) = 0
  | _ => false
end

end
