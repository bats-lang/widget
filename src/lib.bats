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
  | SomeInt of (int)
  | NoneInt

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

#pub datatype ol_list_type =
  | OlDecimal | OlLowerAlpha | OlUpperAlpha
  | OlLowerRoman | OlUpperRoman

#pub datatype img_loading =
  | LoadingLazy | LoadingEager

#pub datatype mime_main =
  | MimeText | MimeImage | MimeAudio | MimeVideo
  | MimeApplication | MimeMultipart | MimeFont
  | MimeMessage | MimeModel

#pub datatype link_target =
  | Blank | Self_ | Parent_ | Top_
  | {n:pos} NamedTarget of ($A.text(n), int(n))

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
  | Ol of (option_int)         (* None=default, Some(n)=ol_list_type index *)
  | Li
  (* Interactive *)
  | {n:pos | n < 256} A of ($A.text(n), int(n), option_int)  (* href, target index *)
  | Button of (button_type)
  | Label of (option_str)      (* for: target id or absent *)
  | Details | Summary
  (* Form *)
  | {n:pos | n < 256} Form of ($A.text(n), int(n), form_method, form_enctype)
  | Fieldset | Legend
  | {n:pos | n < 256} Select of ($A.text(n), int(n), int)    (* name, multiple: 0/1 *)
  | {n:pos | n < 256} Optgroup of ($A.text(n), int(n))       (* label *)
  | {n:pos | n < 256} HtmlOption of ($A.text(n), int(n))     (* value *)
  | {n:pos | n < 256} Textarea of ($A.text(n), int(n), int, int) (* name, rows, cols *)
  (* Table *)
  | Table | Caption | Thead | Tbody | Tfoot | Tr
  | Th of (int, int, option_int)   (* colspan, rowspan, scope index *)
  | Td of (int, int)               (* colspan, rowspan *)
  (* Media *)
  | {n:pos | n < 256} Video of ($A.text(n), int(n), int, int, int, int) (* src, controls, autoplay, loop, muted *)
  | {n:pos | n < 256} Audio of ($A.text(n), int(n), int, int, int, int) (* src, controls, autoplay, loop, muted *)
  | Picture
  (* Metadata *)
  | Style

(* ============================================================
   HTML void elements (no children)
   ============================================================ *)

#pub datatype html_void =
  | Br | Hr | Wbr
  | {ns:pos | ns < 256}{na:pos | na < 256} Img of ($A.text(ns), int(ns), $A.text(na), int(na), img_loading)  (* src, alt, loading *)
  | HtmlInput of (input_type, option_str, option_str, int, int, int) (* type, name, value, disabled, checked, required *)
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

(* A list of n widgets *)
#pub datatype widget_list(int) =
  | WNil(0)
  | {n:nat} WCons(n + 1) of (widget, widget_list(n))

and widget =
  | {n:pos | n < 65536} Text of ($A.text(n), int(n))
  | Element of (element_node)

and element_node =
  | {k:nat} ElementNode of (
      widget_id,    (* id *)
      html_top,     (* element type *)
      int,          (* class index, -1 = none *)
      int,          (* hidden: 0/1 *)
      option_int,   (* tabindex *)
      option_str,   (* title *)
      widget_list(k)   (* children, always WNil when top is Void *)
    )

(* ============================================================
   Diff operations
   ============================================================ *)

#pub datatype diff =
  | RemoveAllChildren of (widget_id)
  | AddChild of (widget_id, widget)       (* parent, child *)
  | RemoveChild of (widget_id, widget_id) (* parent, child_id *)
  | SetHidden of (widget_id, int)
  | {n:pos | n < 256}{i:nat | i < 676} SetClass of (widget_id, int i, $A.text(n), int(n))  (* class index + resolved name *)
  | {n:pos | n < 256} SetClassName of (widget_id, $A.text(n), int(n))   (* set class attr by name *)
  | {n:pos | n < 65536} SetTextContent of (widget_id, $A.text(n), int(n)) (* set text content *)
  | SetTabindex of (widget_id, option_int)
  | SetTitle of (widget_id, option_str)
  | SetAttribute of (widget_id, attribute_change)

and attribute_change =
  (* A *)
  | {n:pos | n < 256} SetHref of ($A.text(n), int(n))
  | SetATarget of (option_int)
  (* Button *)
  | SetButtonType of (button_type)
  | SetButtonDisabled of (int)
  (* Form *)
  | {n:pos | n < 256} SetFormAction of ($A.text(n), int(n))
  | SetFormMethod of (form_method)
  | SetFormEnctype of (form_enctype)
  (* Select *)
  | SetSelectDisabled of (int)
  | SetSelectMultiple of (int)
  (* Option *)
  | {n:pos | n < 256} SetOptionValue of ($A.text(n), int(n))
  | SetOptionDisabled of (int)
  | SetOptionSelected of (int)
  (* Textarea *)
  | {n:pos | n < 256} SetTextareaValue of ($A.text(n), int(n))
  | SetTextareaDisabled of (int)
  | SetTextareaReadonly of (int)
  | SetTextareaRows of (int)
  | SetTextareaCols of (int)
  (* Th, Td *)
  | SetColspan of (int)
  | SetRowspan of (int)
  | SetThScope of (option_int)
  (* Img *)
  | {n:pos | n < 256} SetImgSrc of ($A.text(n), int(n))
  | {n:pos | n < 256} SetImgAlt of ($A.text(n), int(n))
  | SetImgLoading of (img_loading)
  (* Input *)
  | SetInputType of (input_type)
  | SetInputName of (option_str)
  | SetInputValue of (option_str)
  | SetInputDisabled of (int)
  | SetInputChecked of (int)
  | SetInputRequired of (int)
  | SetInputReadonly of (int)
  (* Details *)
  | SetDetailsOpen of (int)

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

#pub fn _wlist_append {n:nat} (wl: widget_list(n), w: widget): widget_list(n + 1)
#pub fn _widget_id_eq(a: widget_id, b: widget_id): bool
#pub fn _wlist_remove_by_id {n:nat}
  (wl: widget_list(n), target: widget_id): [m:nat | m <= n] widget_list(m)

fun _append {n:nat} .<n>. (wl: widget_list(n), w: widget): widget_list(n + 1) =
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
fun _remove {n:nat} .<n>.
  (wl: widget_list(n), target: widget_id): [m:nat | m <= n] widget_list(m) =
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
#pub fn set_hidden(w: widget, h: int): @(widget, diff)
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
    @(Element(ElementNode(id, top, cls, hidden, ti, title, children)),
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
  val style_w = Element(ElementNode(style_id, Normal(Style()), ~1, 0, NoneInt(), NoneStr(), WNil()))
  val @(parent2, d1) = add_child(parent, style_w)
  val d2 = SetTextContent(style_id, css, len)
in @(parent2, DLCons(d1, DLCons(d2, DLNil()))) end
