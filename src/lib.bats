(* widget -- typed HTML widget and diff library *)
(* No $UNSAFE. Algebraic types for HTML elements, widgets, and diffs. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use css as C
#use str as S

(* There is no GC: a datatype's cell with arguments is never freed. Every
   type here whose constructors carry arguments is linear (a datavtype):
   a widget tree and everything in it is consumed by the operation that
   transforms it, by dom's apply (in a diff), or by its free function.
   Types whose constructors carry nothing (button_type, input_type, ...)
   are not allocated, and stay datatypes. *)

(* ============================================================
   Option type for optional values
   ============================================================ *)

#pub datavtype option_int =
  | {v:int} SomeInt of (int v)
  | NoneInt

(* A class: none, or an index into css's class names *)
#pub datavtype class_opt =
  | NoClass
  | {i:nat | i < 676} ClassIdx of (int i)

#pub datavtype option_str =
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
#pub datavtype scope_opt =
  | NoScope
  | ScopeIs of (th_scope)

#pub datatype ol_list_type =
  | OlDecimal | OlLowerAlpha | OlUpperAlpha
  | OlLowerRoman | OlUpperRoman

(* An ordered list's numbering type, when it has one *)
#pub datavtype ol_type_opt =
  | OlDefault
  | OlTypeIs of (ol_list_type)

#pub datatype img_loading =
  | LoadingLazy | LoadingEager

#pub datatype mime_main =
  | MimeText | MimeImage | MimeAudio | MimeVideo
  | MimeApplication | MimeMultipart | MimeFont
  | MimeMessage | MimeModel

#pub datavtype link_target =
  | Blank | Self_ | Parent_ | Top_
  | {n:pos | n < 256} NamedTarget of ($A.text(n), int(n))

(* An anchor's target, when it has one *)
#pub datavtype target_opt =
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

#pub datavtype html_normal =
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

#pub datavtype html_void =
  | Br | Hr | Wbr
  | {ns:pos | ns < 256}{na:pos | na < 256} Img of ($A.text(ns), int(ns), $A.text(na), int(na), img_loading)  (* src, alt, loading *)
  | HtmlInput of (input_type, option_str, option_str, bool, bool, bool) (* type, name, value, disabled, checked, required *)
  | {ns:pos | ns < 256}{nt:pos | nt < 256} Source of ($A.text(ns), int(ns), $A.text(nt), int(nt))  (* src, type *)
  | {n:pos | n < 256} Track of ($A.text(n), int(n), track_kind, option_str) (* src, kind, srclang *)

(* ============================================================
   HTML top type
   ============================================================ *)

#pub datavtype html_top =
  | Normal of (html_normal)
  | Void of (html_void)

(* ============================================================
   Widget ID
   ============================================================ *)

(* An element's id: its text and length, or the empty id, the root (the
   document's mount). Flat: a datatype's cell is never freed (there is no
   GC), and an id is made for every diff. *)
#pub typedef widget_id = [n:nat | n < 256] @($A.text(n), int n)

(* The root's id *)
#pub fn Root (): widget_id

(* The id whose text is t *)
#pub fn Generated {n:pos | n < 256} (t: $A.text(n), n: int n): widget_id

implement Root () = @($A.text_lit(""), 0)

implement Generated (t, n) = @(t, n)

(* ============================================================
   Widget
   ============================================================ *)

(* A list of k widgets whose sizes add up to s. A widget's size is its
   number of nodes (itself and everything under it), so a walk over a
   tree has a termination metric. The tree is linear: widget_free lets
   go of one. *)
#pub datavtype widget_list(int, int) =
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
#pub vtypedef widget = [s:pos] widget_sz(s)

(* ============================================================
   Diff operations
   ============================================================ *)

(* A change to the document. Linear: a datatype's cell is never freed
   (there is no GC); dom's apply consumes each diff. *)
#pub datavtype diff =
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
#pub datavtype diff_seq(int) =
  | DLNil(0)
  | {n:nat} DLCons(n + 1) of (diff, diff_seq(n))

#pub vtypedef diff_list = [n:nat] diff_seq(n)

(* ============================================================
   Letting go of values, and copying them
   ============================================================ *)

(* Each free function lets go of a value and everything in it; each copy
   function makes a new value equal to the one it reads, which it leaves
   as it was. *)

#pub fn option_int_free (o: option_int): void
#pub fn option_int_copy (o: !option_int): option_int
#pub fn class_opt_free (c: class_opt): void
#pub fn class_opt_copy (c: !class_opt): class_opt
#pub fn option_str_free (o: option_str): void
#pub fn option_str_copy (o: !option_str): option_str
#pub fn scope_opt_free (s: scope_opt): void
#pub fn scope_opt_copy (s: !scope_opt): scope_opt
#pub fn ol_type_opt_free (t: ol_type_opt): void
#pub fn ol_type_opt_copy (t: !ol_type_opt): ol_type_opt
#pub fn link_target_free (t: link_target): void
#pub fn link_target_copy (t: !link_target): link_target
#pub fn target_opt_free (t: target_opt): void
#pub fn target_opt_copy (t: !target_opt): target_opt
#pub fn html_normal_free (n: html_normal): void
#pub fn html_normal_copy (n: !html_normal): html_normal
#pub fn html_void_free (v: html_void): void
#pub fn html_void_copy (v: !html_void): html_void
#pub fn html_top_free (t: html_top): void
#pub fn html_top_copy (t: !html_top): html_top

(* A widget tree: the widget and everything under it *)
#pub fn widget_free {s:pos} (w: widget_sz(s)): void
#pub fn widget_copy {s:pos} (w: !widget_sz(s)): widget_sz(s)

(* A list of widgets, each with everything under it *)
#pub fn widget_list_free {k,s:nat} (l: widget_list(k, s)): void
#pub fn widget_list_copy {k,s:nat} (l: !widget_list(k, s)): widget_list(k, s)

(* An attribute change that is not applied *)
#pub fn attribute_change_free (ac: attribute_change): void

implement option_int_free (o) =
  case+ o of
  | ~SomeInt(_) => ()
  | ~NoneInt() => ()

implement option_int_copy (o) =
  case+ o of
  | SomeInt(v) => SomeInt(v)
  | NoneInt() => NoneInt()

implement class_opt_free (c) =
  case+ c of
  | ~ClassIdx(_) => ()
  | ~NoClass() => ()

implement class_opt_copy (c) =
  case+ c of
  | ClassIdx(i) => ClassIdx(i)
  | NoClass() => NoClass()

implement option_str_free (o) =
  case+ o of
  | ~SomeStr(_, _) => ()
  | ~NoneStr() => ()

implement option_str_copy (o) =
  case+ o of
  | SomeStr(t, n) => SomeStr(t, n)
  | NoneStr() => NoneStr()

implement scope_opt_free (s) =
  case+ s of
  | ~ScopeIs(_) => ()
  | ~NoScope() => ()

implement scope_opt_copy (s) =
  case+ s of
  | ScopeIs(x) => ScopeIs(x)
  | NoScope() => NoScope()

implement ol_type_opt_free (t) =
  case+ t of
  | ~OlTypeIs(_) => ()
  | ~OlDefault() => ()

implement ol_type_opt_copy (t) =
  case+ t of
  | OlTypeIs(x) => OlTypeIs(x)
  | OlDefault() => OlDefault()

implement link_target_free (t) =
  case+ t of
  | ~Blank() => () | ~Self_() => () | ~Parent_() => () | ~Top_() => ()
  | ~NamedTarget(_, _) => ()

implement link_target_copy (t) =
  case+ t of
  | Blank() => Blank() | Self_() => Self_() | Parent_() => Parent_() | Top_() => Top_()
  | NamedTarget(x, n) => NamedTarget(x, n)

implement target_opt_free (t) =
  case+ t of
  | ~TargetIs(x) => link_target_free(x)
  | ~NoTarget() => ()

implement target_opt_copy (t) =
  case+ t of
  | TargetIs(x) => TargetIs(link_target_copy(x))
  | NoTarget() => NoTarget()

implement html_normal_free (n) =
  case+ n of
  | ~Div() => () | ~Span() => () | ~Section() => () | ~Article() => ()
  | ~HtmlHeader() => () | ~HtmlFooter() => () | ~HtmlMain() => () | ~Nav() => () | ~Aside() => ()
  | ~H1() => () | ~H2() => () | ~H3() => () | ~H4() => () | ~H5() => () | ~H6() => ()
  | ~P() => () | ~Blockquote() => () | ~Pre() => () | ~HtmlCode() => ()
  | ~Figure() => () | ~Figcaption() => ()
  | ~Strong() => () | ~Em() => () | ~Small() => () | ~Mark() => ()
  | ~Del() => () | ~Ins() => () | ~HtmlSub() => () | ~Sup() => ()
  | ~Ul() => ()
  | ~Ol(t) => ol_type_opt_free(t)
  | ~Li() => ()
  | ~A(_, _, t) => target_opt_free(t)
  | ~Button(_) => ()
  | ~Label(o) => option_str_free(o)
  | ~Details() => () | ~Summary() => ()
  | ~Form(_, _, _, _) => ()
  | ~Fieldset() => () | ~Legend() => ()
  | ~Select(_, _, _) => ()
  | ~Optgroup(_, _) => ()
  | ~HtmlOption(_, _) => ()
  | ~Textarea(_, _, _, _) => ()
  | ~Table() => () | ~Caption() => () | ~Thead() => () | ~Tbody() => () | ~Tfoot() => () | ~Tr() => ()
  | ~Th(_, _, s) => scope_opt_free(s)
  | ~Td(_, _) => ()
  | ~Video(_, _, _, _, _, _) => ()
  | ~Audio(_, _, _, _, _, _) => ()
  | ~Picture() => ()
  | ~Style() => ()

implement html_normal_copy (n) =
  case+ n of
  | Div() => Div() | Span() => Span() | Section() => Section() | Article() => Article()
  | HtmlHeader() => HtmlHeader() | HtmlFooter() => HtmlFooter() | HtmlMain() => HtmlMain()
  | Nav() => Nav() | Aside() => Aside()
  | H1() => H1() | H2() => H2() | H3() => H3() | H4() => H4() | H5() => H5() | H6() => H6()
  | P() => P() | Blockquote() => Blockquote() | Pre() => Pre() | HtmlCode() => HtmlCode()
  | Figure() => Figure() | Figcaption() => Figcaption()
  | Strong() => Strong() | Em() => Em() | Small() => Small() | Mark() => Mark()
  | Del() => Del() | Ins() => Ins() | HtmlSub() => HtmlSub() | Sup() => Sup()
  | Ul() => Ul()
  | Ol(t) => Ol(ol_type_opt_copy(t))
  | Li() => Li()
  | A(h, hl, t) => A(h, hl, target_opt_copy(t))
  | Button(b) => Button(b)
  | Label(o) => Label(option_str_copy(o))
  | Details() => Details() | Summary() => Summary()
  | Form(a, al, m, e) => Form(a, al, m, e)
  | Fieldset() => Fieldset() | Legend() => Legend()
  | Select(x, xl, m) => Select(x, xl, m)
  | Optgroup(x, xl) => Optgroup(x, xl)
  | HtmlOption(x, xl) => HtmlOption(x, xl)
  | Textarea(x, xl, r, c) => Textarea(x, xl, r, c)
  | Table() => Table() | Caption() => Caption() | Thead() => Thead()
  | Tbody() => Tbody() | Tfoot() => Tfoot() | Tr() => Tr()
  | Th(cs, rs, s) => Th(cs, rs, scope_opt_copy(s))
  | Td(cs, rs) => Td(cs, rs)
  | Video(x, xl, c, a, l, m) => Video(x, xl, c, a, l, m)
  | Audio(x, xl, c, a, l, m) => Audio(x, xl, c, a, l, m)
  | Picture() => Picture()
  | Style() => Style()

implement html_void_free (v) =
  case+ v of
  | ~Br() => () | ~Hr() => () | ~Wbr() => ()
  | ~Img(_, _, _, _, _) => ()
  | ~HtmlInput(_, name, value, _, _, _) => let
      val () = option_str_free(name)
    in option_str_free(value) end
  | ~Source(_, _, _, _) => ()
  | ~Track(_, _, _, lang) => option_str_free(lang)

implement html_void_copy (v) =
  case+ v of
  | Br() => Br() | Hr() => Hr() | Wbr() => Wbr()
  | Img(s, sl, a, al, l) => Img(s, sl, a, al, l)
  | HtmlInput(t, name, value, d, c, r) =>
      HtmlInput(t, option_str_copy(name), option_str_copy(value), d, c, r)
  | Source(s, sl, t, tl) => Source(s, sl, t, tl)
  | Track(s, sl, k, lang) => Track(s, sl, k, option_str_copy(lang))

implement html_top_free (t) =
  case+ t of
  | ~Normal(n) => html_normal_free(n)
  | ~Void(v) => html_void_free(v)

implement html_top_copy (t) =
  case+ t of
  | Normal(n) => Normal(html_normal_copy(n))
  | Void(v) => Void(html_void_copy(v))

(* The walks terminate on the tree's size *)
fun _wfree {s:pos} .<s, 0>. (w: widget_sz(s)): void =
  case+ w of
  | ~Text(_, _) => ()
  | ~Element(en) => (case+ en of
    | ~ElementNode(_, top, cls, _, ti, title, kids) => let
        val () = html_top_free(top)
        val () = class_opt_free(cls)
        val () = option_int_free(ti)
        val () = option_str_free(title)
      in _lfree(kids) end)

and _lfree {k,s:nat} .<s, 1>. (l: widget_list(k, s)): void =
  case+ l of
  | ~WNil() => ()
  | ~WCons(w, rest) => let
      val () = _wfree(w)
    in _lfree(rest) end

fun _wcopy {s:pos} .<s, 0>. (w: !widget_sz(s)): widget_sz(s) =
  case+ w of
  | Text(t, n) => Text(t, n)
  | Element(en) => (case+ en of
    | ElementNode(id, top, cls, hidden, ti, title, kids) =>
      Element(ElementNode(id, html_top_copy(top), class_opt_copy(cls), hidden,
        option_int_copy(ti), option_str_copy(title), _lcopy(kids))))

and _lcopy {k,s:nat} .<s, 1>. (l: !widget_list(k, s)): widget_list(k, s) =
  case+ l of
  | WNil() => WNil()
  | WCons(w, rest) => WCons(_wcopy(w), _lcopy(rest))

implement widget_free (w) = _wfree(w)

implement widget_copy (w) = _wcopy(w)

implement widget_list_free (l) = _lfree(l)

implement widget_list_copy (l) = _lcopy(l)

implement attribute_change_free (ac) =
  case+ ac of
  | ~SetHref(_, _) => () | ~SetATarget(t) => target_opt_free(t)
  | ~SetButtonType(_) => () | ~SetButtonDisabled(_) => ()
  | ~SetFormAction(_, _) => () | ~SetFormMethod(_) => () | ~SetFormEnctype(_) => ()
  | ~SetSelectDisabled(_) => () | ~SetSelectMultiple(_) => ()
  | ~SetOptionValue(_, _) => () | ~SetOptionDisabled(_) => () | ~SetOptionSelected(_) => ()
  | ~SetTextareaValue(_, _) => () | ~SetTextareaDisabled(_) => () | ~SetTextareaReadonly(_) => ()
  | ~SetTextareaRows(_) => () | ~SetTextareaCols(_) => ()
  | ~SetColspan(_) => () | ~SetRowspan(_) => () | ~SetThScope(s) => scope_opt_free(s)
  | ~SetImgSrc(_, _) => () | ~SetImgAlt(_, _) => () | ~SetImgLoading(_) => ()
  | ~SetInputType(_) => () | ~SetInputName(o) => option_str_free(o) | ~SetInputValue(o) => option_str_free(o)
  | ~SetInputDisabled(_) => () | ~SetInputChecked(_) => () | ~SetInputRequired(_) => ()
  | ~SetInputReadonly(_) => ()
  | ~SetDetailsOpen(_) => ()

(* ============================================================
   Internal helpers
   ============================================================ *)

(* wl with w appended; both are consumed *)
#pub fn _wlist_append {n,s:nat}{t:pos} (wl: widget_list(n, s), w: widget_sz(t)): widget_list(n + 1, s + t)
#pub fn _widget_id_eq(a: widget_id, b: widget_id): bool
(* wl without its first element whose id is target, which is let go of *)
#pub fn _wlist_remove_by_id {n,s:nat}
  (wl: widget_list(n, s), target: widget_id): [m,s2:nat | m <= n; s2 <= s] widget_list(m, s2)

fun _append {n,s:nat}{t:pos} .<n>. (wl: widget_list(n, s), w: widget_sz(t)): widget_list(n + 1, s + t) =
  case+ wl of
  | ~WNil() => WCons(w, WNil())
  | ~WCons(hd, tl) => WCons(hd, _append(tl, w))

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
implement _widget_id_eq (a, b) = let
  val @(ta, na) = a
  val @(tb, nb) = b
in
  if na = nb then _text_eq(ta, tb, na, 0) else false
end

(* Whether w is an element whose id is target *)
fn _has_id {s:pos} (w: !widget_sz(s), target: widget_id): bool =
  case+ w of
  | Element(en) => (case+ en of
    | ElementNode(id, _, _, _, _, _, _) => _widget_id_eq(id, target))
  | Text(_, _) => false

(* wl without its first element whose id is target *)
fun _remove {n,s:nat} .<n>.
  (wl: widget_list(n, s), target: widget_id): [m,s2:nat | m <= n; s2 <= s] widget_list(m, s2) =
  case+ wl of
  | ~WNil() => WNil()
  | ~WCons(hd, tl) =>
    if _has_id(hd, target) then let
      val () = widget_free(hd)
    in tl end
    else WCons(hd, _remove(tl, target))

implement _wlist_remove_by_id (wl, target) = _remove(wl, target)

fun _dl_free {n:nat} .<n>. (dl: diff_seq(n)): void =
  case+ dl of
  | ~DLNil() => ()
  | ~DLCons(d, rest) => let
      val () = diff_free(d)
    in _dl_free(rest) end

(* ============================================================
   Convenience functions: return (updated_widget, diff)
   ============================================================ *)

(* Each consumes the widget it is given and returns the updated one. A
   Text widget is returned as it was, with a diff aimed at the root. *)

(* Let go of a diff that is not applied *)
#pub fn diff_free (d: diff): void

(* Let go of a diff list that is not applied *)
#pub fn diff_list_free (dl: diff_list): void

(* child is appended to parent's children; the diff holds a copy of it *)
#pub fn add_child(parent: widget, child: widget): @(widget, diff)
(* The removed child is let go of *)
#pub fn remove_child(parent: widget, child_id: widget_id): @(widget, diff)
(* The removed children are let go of *)
#pub fn remove_all_children(w: widget): @(widget, diff)
#pub fn set_hidden(w: widget, h: bool): @(widget, diff)
#pub fn set_class {i:nat | i < 676} (w: widget, cls: int i): @(widget, diff)
#pub fn set_class_name{n:pos | n < 256}(wid: widget_id, cls: $A.text(n), len: int n): diff
#pub fn set_text_content{n:pos | n < 65536}(wid: widget_id, text: $A.text(n), len: int n): diff
(* ti goes into the widget; the diff holds a copy of it *)
#pub fn set_tabindex(w: widget, ti: option_int): @(widget, diff)
(* t goes into the widget; the diff holds a copy of it *)
#pub fn set_title(w: widget, t: option_str): @(widget, diff)
#pub fn inject_css{n:pos | n < 65536}(parent: widget, style_id: widget_id, css: $A.text(n), len: int n): @(widget, diff_list)

implement diff_free (d) =
  case+ d of
  | ~RemoveAllChildren(_) => ()
  | ~AddChild(_, w) => widget_free(w)
  | ~RemoveChild(_, _) => ()
  | ~SetHidden(_, _) => ()
  | ~SetClass(_, _, _, _) => ()
  | ~SetClassName(_, _, _) => ()
  | ~SetTextContent(_, _, _) => ()
  | ~SetTabindex(_, ti) => option_int_free(ti)
  | ~SetTitle(_, t) => option_str_free(t)
  | ~SetAttribute(_, ac) => attribute_change_free(ac)

implement diff_list_free (dl) = _dl_free(dl)

implement add_child (parent, child) =
  case+ parent of
  | Text(_, _) => @(parent, AddChild(Root(), child))
  | ~Element(en) => (case+ en of
    | ~ElementNode(id, top, cls, hidden, ti, title, children) => let
        val c2 = widget_copy(child)
      in
        @(Element(ElementNode(id, top, cls, hidden, ti, title, _append(children, child))),
          AddChild(id, c2))
      end)

implement remove_child (parent, child_id) =
  case+ parent of
  | Text(_, _) => @(parent, RemoveChild(Root(), child_id))
  | ~Element(en) => (case+ en of
    | ~ElementNode(id, top, cls, hidden, ti, title, children) =>
      @(Element(ElementNode(id, top, cls, hidden, ti, title, _remove(children, child_id))),
        RemoveChild(id, child_id)))

implement remove_all_children (w) =
  case+ w of
  | Text(_, _) => @(w, RemoveAllChildren(Root()))
  | ~Element(en) => (case+ en of
    | ~ElementNode(id, top, cls, hidden, ti, title, children) => let
        val () = widget_list_free(children)
      in
        @(Element(ElementNode(id, top, cls, hidden, ti, title, WNil())),
          RemoveAllChildren(id))
      end)

implement set_hidden (w, h) =
  case+ w of
  | Text(_, _) => @(w, SetHidden(Root(), h))
  | ~Element(en) => (case+ en of
    | ~ElementNode(id, top, cls, _, ti, title, children) =>
      @(Element(ElementNode(id, top, cls, h, ti, title, children)),
        SetHidden(id, h)))

implement set_class (w, cls) = let
  val @(t, tlen) = $C.class_text(cls)
in
  case+ w of
  | Text(_, _) => @(w, SetClass(Root(), cls, t, tlen))
  | ~Element(en) => (case+ en of
    | ~ElementNode(id, top, old, hidden, ti, title, children) => let
        val () = class_opt_free(old)
      in
        @(Element(ElementNode(id, top, ClassIdx(cls), hidden, ti, title, children)),
          SetClass(id, cls, t, tlen))
      end)
end

implement set_tabindex (w, ti) =
  case+ w of
  | Text(_, _) => @(w, SetTabindex(Root(), ti))
  | ~Element(en) => (case+ en of
    | ~ElementNode(id, top, cls, hidden, old, title, children) => let
        val () = option_int_free(old)
        val ti2 = option_int_copy(ti)
      in
        @(Element(ElementNode(id, top, cls, hidden, ti, title, children)),
          SetTabindex(id, ti2))
      end)

implement set_class_name (wid, cls, len) = SetClassName(wid, cls, len)

implement set_text_content (wid, text, len) = SetTextContent(wid, text, len)

implement set_title (w, t) =
  case+ w of
  | Text(_, _) => @(w, SetTitle(Root(), t))
  | ~Element(en) => (case+ en of
    | ~ElementNode(id, top, cls, hidden, ti, old, children) => let
        val () = option_str_free(old)
        val t2 = option_str_copy(t)
      in
        @(Element(ElementNode(id, top, cls, hidden, ti, t, children)),
          SetTitle(id, t2))
      end)

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

fun wlist_len {n,s:nat} .<n>. (wl: !widget_list(n, s)): int n =
  case+ wl of
  | WNil() => 0
  | WCons(_, rest) => 1 + wlist_len(rest)

(* The number of children of an element, or ~1 for text *)
fn nkids (w: !widget): int =
  case+ w of
  | Element(en) => (case+ en of ElementNode(_, _, _, _, _, _, ch) => wlist_len(ch))
  | Text(_, _) => ~1

(* ok, having let go of w (ok is computed from w before it goes) *)
fn then_wfree(ok: bool, w: widget): bool = let val () = widget_free(w) in ok end

(* ok, having let go of d (ok is computed from d before it goes) *)
fn then_free(ok: bool, d: diff): bool = let val () = diff_free(d) in ok end

(* ---- apply_diff: a model of what a diff does to a widget ---- *)

(* The id a diff is aimed at *)
fn diff_target(d: !diff): widget_id =
  case+ d of
  | RemoveAllChildren(t) => t | AddChild(t, _) => t | RemoveChild(t, _) => t
  | SetHidden(t, _) => t | SetClass(t, _, _, _) => t | SetClassName(t, _, _) => t
  | SetTextContent(t, _, _) => t | SetTabindex(t, _) => t | SetTitle(t, _) => t
  | SetAttribute(t, _) => t

fn _apply_diff(w: widget, d: !diff): widget =
  case+ w of
  | Text(_, _) => w
  | ~Element(en) => (case+ en of
    | ~ElementNode(id, top, cls, hidden, ti, title, kids) =>
      if ~widget_id_eq(id, diff_target(d)) then
        Element(ElementNode(id, top, cls, hidden, ti, title, kids))
      else (case+ d of
      | SetHidden(_, h) => Element(ElementNode(id, top, cls, h, ti, title, kids))
      | SetClass(_, c, _, _) => let
          val () = class_opt_free(cls)
        in Element(ElementNode(id, top, ClassIdx(c), hidden, ti, title, kids)) end
      | SetTabindex(_, t) => let
          val () = option_int_free(ti)
        in Element(ElementNode(id, top, cls, hidden, option_int_copy(t), title, kids)) end
      | SetTitle(_, t) => let
          val () = option_str_free(title)
        in Element(ElementNode(id, top, cls, hidden, ti, option_str_copy(t), kids)) end
      | RemoveAllChildren(_) => let
          val () = widget_list_free(kids)
        in Element(ElementNode(id, top, cls, hidden, ti, title, WNil())) end
      | AddChild(_, c) =>
          Element(ElementNode(id, top, cls, hidden, ti, title, _wlist_append(kids, widget_copy(c))))
      | RemoveChild(_, cid) =>
          Element(ElementNode(id, top, cls, hidden, ti, title, _wlist_remove_by_id(kids, cid)))
      (* class name and text content are DOM-only concepts; attribute
         changes require html_top mutation *)
      | SetClassName(_, _, _) => Element(ElementNode(id, top, cls, hidden, ti, title, kids))
      | SetTextContent(_, _, _) => Element(ElementNode(id, top, cls, hidden, ti, title, kids))
      | SetAttribute(_, _) => Element(ElementNode(id, top, cls, hidden, ti, title, kids))))

fn apply_diff(w: widget, d: diff): widget = let
  val r = _apply_diff(w, d)
  val () = diff_free(d)
in r end

fn class_eq(a: !class_opt, b: !class_opt): bool =
  case+ a of
  | NoClass() => (case+ b of | NoClass() => true | ClassIdx(_) => false)
  | ClassIdx(i) => (case+ b of | ClassIdx(j) => $AR.eq_int_int(i, j) | NoClass() => false)

fn widget_eq(a: !widget, b: !widget): bool =
  case+ a of
  | Text(_, l1) => (case+ b of | Text(_, l2) => $AR.eq_int_int(l1, l2) | Element(_) => false)
  | Element(ea) => (case+ ea of
    | ElementNode(id1, _, c1, h1, _, _, ch1) =>
      (case+ b of
      | Text(_, _) => false
      | Element(eb) => (case+ eb of
        | ElementNode(id2, _, c2, h2, _, _, ch2) =>
            widget_id_eq(id1, id2) &&
            class_eq(c1, c2) &&
            h1 = h2 &&
            $AR.eq_int_int(wlist_len(ch1), wlist_len(ch2)))))

(* a and b are equal; both are let go of *)
fn eq_free(a: widget, b: widget): bool = let
  val ok = widget_eq(a, b)
  val () = widget_free(a)
  val () = widget_free(b)
in ok end

fn mk(top: html_top): widget =
  Element(ElementNode(Root(), top, NoClass(), false, NoneInt(), NoneStr(), WNil()))

(* w's children number n; w is let go of *)
fn kids_free(w: widget, n: int): bool = then_wfree(nkids(w) = n, w)

(* ---- Round-trip proofs ---- *)

fn test_proof_set_hidden(): bool = let
  val w = mk(Normal(Div()))
  val d = SetHidden(Root(), true)
  val result = apply_diff(w, d)
  val expected = Element(ElementNode(Root(), Normal(Div()), NoClass(), true, NoneInt(), NoneStr(), WNil()))
in eq_free(result, expected) end

fn test_proof_hidden_reversible(): bool = let
  val w = mk(Normal(Div()))
  val hidden = apply_diff(widget_copy(w), SetHidden(Root(), true))
  val restored = apply_diff(hidden, SetHidden(Root(), false))
in eq_free(restored, w) end

fn test_proof_hidden_idempotent(): bool = let
  val w = mk(Normal(Div()))
  val w1 = apply_diff(w, SetHidden(Root(), true))
  val w2 = apply_diff(widget_copy(w1), SetHidden(Root(), true))
in eq_free(w1, w2) end

fn mk_set_class {i:nat | i < 676} (wid: widget_id, cls: int i): diff = let
  val @(t, tlen) = $C.class_text(cls)
in SetClass(wid, cls, t, tlen) end

fn test_proof_set_class(): bool = let
  val w = mk(Normal(Span()))
  val result = apply_diff(w, mk_set_class(Root(), 3))
  val expected = Element(ElementNode(Root(), Normal(Span()), ClassIdx(3), false, NoneInt(), NoneStr(), WNil()))
in eq_free(result, expected) end

fn test_proof_class_replaces(): bool = let
  val w = mk(Normal(P()))
  val w1 = apply_diff(w, mk_set_class(Root(), 5))
  val w2 = apply_diff(w1, mk_set_class(Root(), 9))
  val expected = Element(ElementNode(Root(), Normal(P()), ClassIdx(9), false, NoneInt(), NoneStr(), WNil()))
in eq_free(w2, expected) end

fn test_proof_compose_commutes(): bool = let
  val w = mk(Normal(Nav()))
  val a = apply_diff(apply_diff(widget_copy(w), SetHidden(Root(), true)), mk_set_class(Root(), 2))
  val b = apply_diff(apply_diff(w, mk_set_class(Root(), 2)), SetHidden(Root(), true))
in eq_free(a, b) end

fn test_proof_add_child(): bool = let
  val w = mk(Normal(Div()))
  val child = txt_widget5('h', 'e', 'l', 'l', 'o')
  val result = apply_diff(w, AddChild(Root(), child))
in kids_free(result, 1) end

fn test_proof_add_two_children(): bool = let
  val w = mk(Normal(Ul()))
  val w1 = apply_diff(w, AddChild(Root(), txt_widget5('f', 'i', 'r', 's', 't')))
  val w2 = apply_diff(w1, AddChild(Root(), txt_widget1('s')))
in kids_free(w2, 2) end

fn test_proof_remove_all_children(): bool = let
  val w = mk(Normal(Div()))
  val w1 = apply_diff(w, AddChild(Root(), txt_widget1('a')))
  val w2 = apply_diff(w1, AddChild(Root(), txt_widget1('b')))
  val w3 = apply_diff(w2, RemoveAllChildren(Root()))
in kids_free(w3, 0) end

fn test_proof_remove_all_then_add(): bool = let
  val w = mk(Normal(Div()))
  val w1 = apply_diff(w, AddChild(Root(), txt_widget1('o')))
  val w2 = apply_diff(w1, RemoveAllChildren(Root()))
  val w3 = apply_diff(w2, AddChild(Root(), txt_widget1('n')))
in kids_free(w3, 1) end

fn test_proof_text_ignores_diff(): bool = let
  val w = txt_widget1('u')
  val w1 = apply_diff(widget_copy(w), SetHidden(Root(), true))
  val w2 = apply_diff(widget_copy(w), mk_set_class(Root(), 5))
  val w3 = apply_diff(widget_copy(w), AddChild(Root(), txt_widget1('x')))
  val w4 = apply_diff(widget_copy(w), RemoveAllChildren(Root()))
  val e1 = widget_eq(w1, w)
  val e2 = widget_eq(w2, w)
  val e3 = widget_eq(w3, w)
  val e4 = widget_eq(w4, w)
  val () = widget_free(w1)
  val () = widget_free(w2)
  val () = widget_free(w3)
  val () = widget_free(w4)
in then_wfree(e1 && e2 && e3 && e4, w) end

fn test_proof_wrong_id_noop(): bool = let
  val @(t, n) = mk_text2('g', '1')
  val w = mk(Normal(Div()))
  (* A generated id never matches the root *)
  val result = apply_diff(w, SetHidden(Generated(t, n), true))
  val expected = mk(Normal(Div()))
in eq_free(result, expected) end

fn test_proof_set_tabindex(): bool = let
  val w = mk(Normal(Div()))
  val result = apply_diff(w, SetTabindex(Root(), SomeInt(0)))
  val ok = (case+ result of
    | Element(en) => (case+ en of
      | ElementNode(_, _, _, _, ti, _, _) =>
        (case+ ti of | SomeInt(v) => $AR.eq_int_int(v, 0) | NoneInt() => false))
    | Text(_, _) => false)
in then_wfree(ok, result) end

fn test_proof_set_title(): bool = let
  val w = mk(Normal(Button(ButtonSubmit())))
  val @(ct, clen) = mk_text5('c', 'l', 'i', 'c', 'k')
  val result = apply_diff(w, SetTitle(Root(), SomeStr(ct, clen)))
  val ok = (case+ result of
    | Element(en) => (case+ en of
      | ElementNode(_, _, _, _, _, t, _) =>
        (case+ t of | SomeStr(_, n) => $AR.eq_int_int(n, 5) | NoneStr() => false))
    | Text(_, _) => false)
in then_wfree(ok, result) end

(* ---- Datatype construction tests ---- *)

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
  val ok1 = case+ n of | Normal(_) => true | Void(_) => false
  val ok2 = case+ v of | Void(_) => true | Normal(_) => false
  val () = html_top_free(n)
  val () = html_top_free(v)
in ok1 && ok2 end

fn test_element_node(): bool = let
  val e = Element(ElementNode(Root(), Normal(Div()), NoClass(), false, NoneInt(), NoneStr(), WNil()))
  val ok = (case+ e of
    | Element(en) => (case+ en of ElementNode(id, _, _, _, _, _, _) => widget_id_eq(id, Root()))
    | Text(_, _) => false)
in then_wfree(ok, e) end

fn test_widget_with_children(): bool = let
  val children = WCons(txt_widget1('a'), WCons(txt_widget1('b'), WNil()))
  val e = Element(ElementNode(Root(), Normal(Ul()), NoClass(), false, NoneInt(), NoneStr(), children))
in kids_free(e, 2) end

fn test_label(): bool = let
  val @(ft, flen) = mk_text3('f', 'o', 'o')
  val l = Label(SomeStr(ft, flen))
  val ok = (case+ l of
    | Label(s) => (case+ s of | SomeStr(_, n) => $AR.eq_int_int(n, 3) | NoneStr() => false)
    | _ => false)
  val () = html_normal_free(l)
in ok end

fn test_optgroup(): bool = let
  val @(t, tl) = mk_text3('r', 'e', 'd')
  val og = Optgroup(t, tl)
  val ok = (case+ og of | Optgroup(_, n) => $AR.eq_int_int(n, 3) | _ => false)
  val () = html_normal_free(og)
in ok end

fn test_th_with_scope(): bool = let
  val th = Th(2, 3, ScopeIs(ScopeRow()))
  val ok = (case+ th of | Th(cs, rs, _) => $AR.eq_int_int(cs, 2) && $AR.eq_int_int(rs, 3) | _ => false)
  val () = html_normal_free(th)
in ok end

fn test_ol_with_type(): bool = let
  val ol = Ol(OlTypeIs(OlLowerAlpha()))
  val ok = (case+ ol of
    | Ol(t) => (case+ t of | OlTypeIs(OlLowerAlpha()) => true | _ => false)
    | _ => false)
  val () = html_normal_free(ol)
in ok end

fn test_diff_set_attribute(): bool = let
  val @(ht, hlen) = mk_text3('u', 'r', 'l')
  val d = SetAttribute(Root(), SetHref(ht, hlen))
in then_free((case+ d of | SetAttribute(_, ac) => (case+ ac of | SetHref(_, _) => true | _ => false) | _ => false), d) end

(* A copy is equal to what it copies, and is its own: changing the copy
   leaves the original as it was *)
fn test_copy_independent(): bool = let
  val w = mk(Normal(Div()))
  val w1 = apply_diff(w, AddChild(Root(), txt_widget1('a')))
  val c = widget_copy(w1)
  val same = widget_eq(c, w1)
  val c2 = apply_diff(c, AddChild(Root(), txt_widget1('b')))
  val k2 = nkids(c2)
  val k1 = nkids(w1)
  val ok = same && k2 = 2 && k1 = 1
  val () = widget_free(c2)
in then_wfree(ok, w1) end

(* ---- Convenience function tests ---- *)

fn test_conv_add_child(): bool = let
  val w = mk(Normal(Div()))
  val @(w2, d) = add_child(w, txt_widget5('h', 'e', 'l', 'l', 'o'))
  val okd = then_free((case+ d of | AddChild(id, c) => widget_id_eq(id, Root()) && nkids(c) = ~1 | _ => false), d)
in kids_free(w2, 1) && okd end

fn test_conv_set_hidden(): bool = let
  val w = mk(Normal(Div()))
  val @(w2, d) = set_hidden(w, true)
  val okd = then_free((case+ d of | SetHidden(_, v) => v | _ => false), d)
  val ok = (case+ w2 of
    | Element(en) => (case+ en of ElementNode(_, _, _, h, _, _, _) => h)
    | Text(_, _) => false)
in then_wfree(ok, w2) && okd end

fn test_conv_set_class(): bool = let
  val w = mk(Normal(Span()))
  val @(w2, d) = set_class(w, 7)
  val okd = then_free((case+ d of | SetClass(_, v, _, _) => $AR.eq_int_int(v, 7) | _ => false), d)
  val ok = (case+ w2 of
    | Element(en) => (case+ en of
      | ElementNode(_, _, c, _, _, _, _) => (case+ c of ClassIdx(i) => i = 7 | NoClass() => false))
    | Text(_, _) => false)
in then_wfree(ok, w2) && okd end

fn test_conv_remove_all_children(): bool = let
  val w = mk(Normal(Div()))
  val @(w1, d1) = add_child(w, txt_widget1('a'))
  val () = diff_free(d1)
  val @(w2, d2) = add_child(w1, txt_widget1('b'))
  val () = diff_free(d2)
  val @(w3, d) = remove_all_children(w2)
  val okd = then_free((case+ d of | RemoveAllChildren(_) => true | _ => false), d)
in kids_free(w3, 0) && okd end

fn test_conv_text_noop(): bool = let
  val w = txt_widget2('h', 'i')
  val @(w2, d3) = set_hidden(widget_copy(w), true)
  val () = diff_free(d3)
in eq_free(w, w2) end

(* Generated ids are compared by their text: two ids with the same text
   are the same id, so remove_child finds the child *)
fn test_remove_child_by_generated_id(): bool = let
  val @(t1, n1) = mk_text2('b', '1')
  val @(t2, n2) = mk_text2('b', '1')
  val kid = Element(ElementNode(Generated(t1, n1), Normal(Div()), NoClass(), false, NoneInt(), NoneStr(), WNil()))
  val @(w1, d4) = add_child(mk(Normal(Div())), kid)
  val () = diff_free(d4)
  val @(w2, d5) = remove_child(w1, Generated(t2, n2))
  val () = diff_free(d5)
in kids_free(w2, 0) end

end
