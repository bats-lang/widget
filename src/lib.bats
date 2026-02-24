(* widget -- typed HTML widget and diff library *)
(* No $UNSAFE. Algebraic types for HTML elements, widgets, and diffs. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use css as C

(* ============================================================
   Option type for optional values
   ============================================================ *)

#pub datatype option_int =
  | SomeInt of (int)
  | NoneInt

#pub datatype option_str =
  | SomeStr of (string)
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
  | A of (string, option_int)  (* href, target index *)
  | Button of (button_type)
  | Label of (option_str)      (* for: target id or absent *)
  | Details | Summary
  (* Form *)
  | Form of (string, form_method, form_enctype)
  | Fieldset | Legend
  | Select of (string, int)    (* name, multiple: 0/1 *)
  | Optgroup of (string)       (* label *)
  | HtmlOption of (string)     (* value *)
  | Textarea of (string, int, int) (* name, rows, cols *)
  (* Table *)
  | Table | Caption | Thead | Tbody | Tfoot | Tr
  | Th of (int, int, option_int)   (* colspan, rowspan, scope index *)
  | Td of (int, int)               (* colspan, rowspan *)
  (* Media *)
  | Video of (string, int, int, int, int) (* src, controls, autoplay, loop, muted *)
  | Audio of (string, int, int, int, int) (* src, controls, autoplay, loop, muted *)
  | Picture

(* ============================================================
   HTML void elements (no children)
   ============================================================ *)

#pub datatype html_void =
  | Br | Hr | Wbr
  | Img of (string, string, img_loading)  (* src, alt, loading *)
  | HtmlInput of (input_type, option_str, option_str, int, int, int) (* type, name, value, disabled, checked, required *)
  | Source of (string, string)  (* src, type *)
  | Track of (string, track_kind, option_str) (* src, kind, srclang *)

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
  | {n:pos} Generated of ($A.text(n), int(n))

(* ============================================================
   Widget
   ============================================================ *)

#pub datatype widget_list =
  | WNil
  | WCons of (widget, widget_list)

#pub datatype widget =
  | Text of (string)
  | Element of (element_node)

#pub datatype element_node =
  | ElementNode of (
      widget_id,    (* id *)
      html_top,     (* element type *)
      int,          (* class index, -1 = none *)
      int,          (* hidden: 0/1 *)
      option_int,   (* tabindex *)
      option_str,   (* title *)
      widget_list   (* children, always WNil when top is Void *)
    )

(* ============================================================
   Diff operations
   ============================================================ *)

#pub datatype diff =
  | RemoveAllChildren of (widget_id)
  | AddChild of (widget_id, widget)       (* parent, child *)
  | RemoveChild of (widget_id, widget_id) (* parent, child_id *)
  | SetHidden of (widget_id, int)
  | SetClass of (widget_id, int)          (* -1 = none *)
  | SetTabindex of (widget_id, option_int)
  | SetTitle of (widget_id, option_str)
  | SetAttribute of (widget_id, attribute_change)

(* ============================================================
   Attribute changes
   ============================================================ *)

#pub datatype attribute_change =
  (* A *)
  | SetHref of (string)
  | SetATarget of (option_int)
  (* Button *)
  | SetButtonType of (button_type)
  | SetButtonDisabled of (int)
  (* Form *)
  | SetFormAction of (string)
  | SetFormMethod of (form_method)
  | SetFormEnctype of (form_enctype)
  (* Select *)
  | SetSelectDisabled of (int)
  | SetSelectMultiple of (int)
  (* Option *)
  | SetOptionValue of (string)
  | SetOptionDisabled of (int)
  | SetOptionSelected of (int)
  (* Textarea *)
  | SetTextareaValue of (string)
  | SetTextareaDisabled of (int)
  | SetTextareaReadonly of (int)
  | SetTextareaRows of (int)
  | SetTextareaCols of (int)
  (* Th, Td *)
  | SetColspan of (int)
  | SetRowspan of (int)
  | SetThScope of (option_int)
  (* Img *)
  | SetImgSrc of (string)
  | SetImgAlt of (string)
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
   Unit tests
   ============================================================ *)

$UNITTEST.run begin

(* ---- Helpers ---- *)

fn widget_id_eq(a: widget_id, b: widget_id): bool =
  case+ a of
  | Root() => (case+ b of | Root() => true | _ => false)
  | Generated(_, _) => false

fn wlist_len(wl: widget_list): int =
  case+ wl of
  | WNil() => 0
  | WCons(_, rest) => 1 + wlist_len(rest)

fn wlist_append(wl: widget_list, w: widget): widget_list =
  case+ wl of
  | WNil() => WCons(w, WNil())
  | WCons(hd, tl) => WCons(hd, wlist_append(tl, w))

fn wlist_remove_by_id(wl: widget_list, target: widget_id): widget_list =
  case+ wl of
  | WNil() => WNil()
  | WCons(hd, tl) => let
      val matches = case+ hd of
        | Element(ElementNode(id, _, _, _, _, _, _)) => widget_id_eq(id, target)
        | Text(_) => false
    in
      if matches then tl
      else WCons(hd, wlist_remove_by_id(tl, target))
    end

(* ---- apply_diff ---- *)

fn apply_diff(w: widget, d: diff): widget =
  case+ w of
  | Text(_) => w
  | Element(ElementNode(id, top, cls, hidden, tabidx, title, children)) =>
    case+ d of
    | SetHidden(target, new_h) =>
        if widget_id_eq(id, target)
        then Element(ElementNode(id, top, cls, new_h, tabidx, title, children))
        else w
    | SetClass(target, new_cls) =>
        if widget_id_eq(id, target)
        then Element(ElementNode(id, top, new_cls, hidden, tabidx, title, children))
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
    | SetAttribute(_, _) => w  (* attribute changes require html_top mutation *)

fn widget_eq(a: widget, b: widget): bool =
  case+ a of
  | Text(s1) => (case+ b of | Text(s2) => s1 = s2 | _ => false)
  | Element(ElementNode(id1, _, c1, h1, _, _, ch1)) =>
    (case+ b of
    | Text(_) => false
    | Element(ElementNode(id2, _, c2, h2, _, _, ch2)) =>
        widget_id_eq(id1, id2) &&
        $AR.eq_int_int(c1, c2) &&
        $AR.eq_int_int(h1, h2) &&
        $AR.eq_int_int(wlist_len(ch1), wlist_len(ch2)))

fn mk(top: html_top): widget =
  Element(ElementNode(Root(), top, ~1, 0, NoneInt(), NoneStr(), WNil()))

(* ---- Round-trip proofs ---- *)

fn test_proof_set_hidden(): bool = let
  val w = mk(Normal(Div()))
  val d = SetHidden(Root(), 1)
  val result = apply_diff(w, d)
  val expected = Element(ElementNode(Root(), Normal(Div()), ~1, 1, NoneInt(), NoneStr(), WNil()))
in widget_eq(result, expected) end

fn test_proof_hidden_reversible(): bool = let
  val w = mk(Normal(Div()))
  val hidden = apply_diff(w, SetHidden(Root(), 1))
  val restored = apply_diff(hidden, SetHidden(Root(), 0))
in widget_eq(restored, w) end

fn test_proof_hidden_idempotent(): bool = let
  val w = mk(Normal(Div()))
  val d = SetHidden(Root(), 1)
  val w1 = apply_diff(w, d)
  val w2 = apply_diff(w1, d)
in widget_eq(w1, w2) end

fn test_proof_set_class(): bool = let
  val w = mk(Normal(Span()))
  val result = apply_diff(w, SetClass(Root(), 3))
  val expected = Element(ElementNode(Root(), Normal(Span()), 3, 0, NoneInt(), NoneStr(), WNil()))
in widget_eq(result, expected) end

fn test_proof_class_replaces(): bool = let
  val w = mk(Normal(P()))
  val w1 = apply_diff(w, SetClass(Root(), 5))
  val w2 = apply_diff(w1, SetClass(Root(), 9))
  val expected = Element(ElementNode(Root(), Normal(P()), 9, 0, NoneInt(), NoneStr(), WNil()))
in widget_eq(w2, expected) end

fn test_proof_compose_commutes(): bool = let
  val w = mk(Normal(Nav()))
  val a = apply_diff(apply_diff(w, SetHidden(Root(), 1)), SetClass(Root(), 2))
  val b = apply_diff(apply_diff(w, SetClass(Root(), 2)), SetHidden(Root(), 1))
in widget_eq(a, b) end

fn test_proof_add_child(): bool = let
  val w = mk(Normal(Div()))
  val child = Text("hello")
  val result = apply_diff(w, AddChild(Root(), child))
in
  case+ result of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => $AR.eq_int_int(wlist_len(ch), 1)
  | _ => false
end

fn test_proof_add_two_children(): bool = let
  val w = mk(Normal(Ul()))
  val w1 = apply_diff(w, AddChild(Root(), Text("first")))
  val w2 = apply_diff(w1, AddChild(Root(), Text("second")))
in
  case+ w2 of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => $AR.eq_int_int(wlist_len(ch), 2)
  | _ => false
end

fn test_proof_remove_all_children(): bool = let
  val w = mk(Normal(Div()))
  val w1 = apply_diff(w, AddChild(Root(), Text("a")))
  val w2 = apply_diff(w1, AddChild(Root(), Text("b")))
  val w3 = apply_diff(w2, RemoveAllChildren(Root()))
in
  case+ w3 of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => $AR.eq_int_int(wlist_len(ch), 0)
  | _ => false
end

fn test_proof_remove_all_then_add(): bool = let
  val w = mk(Normal(Div()))
  val w1 = apply_diff(w, AddChild(Root(), Text("old")))
  val w2 = apply_diff(w1, RemoveAllChildren(Root()))
  val w3 = apply_diff(w2, AddChild(Root(), Text("new")))
in
  case+ w3 of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => $AR.eq_int_int(wlist_len(ch), 1)
  | _ => false
end

fn test_proof_text_ignores_diff(): bool = let
  val w = Text("unchanged")
  val w1 = apply_diff(w, SetHidden(Root(), 1))
  val w2 = apply_diff(w, SetClass(Root(), 5))
  val w3 = apply_diff(w, AddChild(Root(), Text("x")))
  val w4 = apply_diff(w, RemoveAllChildren(Root()))
in widget_eq(w1, w) && widget_eq(w2, w) && widget_eq(w3, w) && widget_eq(w4, w) end

fn test_proof_wrong_id_noop(): bool = let
  val w = mk(Normal(Div()))
  (* Generated IDs never match Root *)
  val d = SetHidden(Root(), 1)
  val result = apply_diff(w, d)
  val expected = Element(ElementNode(Root(), Normal(Div()), ~1, 1, NoneInt(), NoneStr(), WNil()))
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
  val result = apply_diff(w, SetTitle(Root(), SomeStr("Click me")))
in
  case+ result of
  | Element(ElementNode(_, _, _, _, _, t, _)) =>
    (case+ t of | SomeStr(s) => s = "Click me" | _ => false)
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
  val e = ElementNode(Root(), Normal(Div()), ~1, 0, NoneInt(), NoneStr(), WNil())
in case+ e of | ElementNode(id, _, _, _, _, _, _) => widget_id_eq(id, Root()) end

fn test_widget_with_children(): bool = let
  val children = WCons(Text("a"), WCons(Text("b"), WNil()))
  val e = Element(ElementNode(Root(), Normal(Ul()), ~1, 0, NoneInt(), NoneStr(), children))
in case+ e of
  | Element(ElementNode(_, _, _, _, _, _, ch)) => $AR.eq_int_int(wlist_len(ch), 2)
  | _ => false
end

fn test_label(): bool = let
  val l = Label(SomeStr("field-1"))
in case+ l of | Label(s) => (case+ s of | SomeStr(v) => v = "field-1" | _ => false) | _ => false end

fn test_optgroup(): bool = let
  val og = Optgroup("Colors")
in case+ og of | Optgroup(lbl) => lbl = "Colors" | _ => false end

fn test_th_with_scope(): bool = let
  val th = Th(2, 3, SomeInt(1))
in case+ th of | Th(cs, rs, _) => $AR.eq_int_int(cs, 2) && $AR.eq_int_int(rs, 3) | _ => false end

fn test_ol_with_type(): bool = let
  val ol = Ol(SomeInt(1))
in case+ ol of | Ol(t) => (case+ t of | SomeInt(v) => $AR.eq_int_int(v, 1) | _ => false) | _ => false end

fn test_diff_set_attribute(): bool = let
  val d = SetAttribute(Root(), SetHref("https://example.com"))
in case+ d of | SetAttribute(_, ac) => (case+ ac of | SetHref(_) => true | _ => false) | _ => false end

end
