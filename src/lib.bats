(* widget -- typed HTML widget and diff library *)
(* No $UNSAFE. Algebraic types for HTML elements, widgets, and diffs. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use css as C

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
  | Ol of (int)  (* 0=default, 1=decimal, 2=lowerAlpha, etc. *)
  | Li
  (* Interactive *)
  | A of (string, int)  (* href, target: 0=none,1=blank,2=self,3=parent,4=top *)
  | Button of (button_type)
  | Details | Summary
  (* Form *)
  | Form of (string, form_method, form_enctype)
  | Fieldset | Legend
  | Select of (string, int)  (* name, multiple: 0/1 *)
  | HtmlOption of (string)  (* value *)
  | Textarea of (string, int, int) (* name, rows, cols *)
  (* Table *)
  | Table | Caption | Thead | Tbody | Tfoot | Tr
  | Th of (int, int, int)  (* colspan, rowspan, scope: 0=none *)
  | Td of (int, int)  (* colspan, rowspan *)
  (* Media *)
  | Video of (string, int, int) (* src, controls, autoplay *)
  | Audio of (string, int, int) (* src, controls, autoplay *)
  | Picture

(* ============================================================
   HTML void elements (no children)
   ============================================================ *)

#pub datatype html_void =
  | Br | Hr | Wbr
  | Img of (string, string, img_loading)  (* src, alt, loading *)
  | HtmlInput of (input_type, string, int) (* type, name, disabled *)
  | Source of (string, string)  (* src, type *)
  | Track of (string, track_kind) (* src, kind *)

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

#pub datatype widget =
  | Text of (string)
  | Element of (widget_id, html_top, int, int) (* id, top, hidden, class_idx *)

(* ============================================================
   Diff operations
   ============================================================ *)

#pub datatype diff =
  | RemoveAllChildren of (widget_id)
  | AddChild of (widget_id, widget)  (* parent, child *)
  | RemoveChild of (widget_id, widget_id) (* parent, child_id *)
  | SetHidden of (widget_id, int)  (* target, hidden *)
  | SetClass of (widget_id, int)  (* target, class_idx: -1=none *)

(* ============================================================
   Attribute changes
   ============================================================ *)

#pub datatype attribute_change =
  (* A *)
  | SetHref of (string)
  | SetATarget of (int)
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
  | SetThScope of (int)
  (* Img *)
  | SetImgSrc of (string)
  | SetImgAlt of (string)
  | SetImgLoading of (img_loading)
  (* Input *)
  | SetInputType of (input_type)
  | SetInputValue of (string)
  | SetInputDisabled of (int)
  | SetInputChecked of (int)
  | SetInputRequired of (int)
  (* Details *)
  | SetDetailsOpen of (int)

(* ============================================================
   Unit tests
   ============================================================ *)

$UNITTEST.run begin

fn test_rel_values(): bool = let
  val r1 = RelNoopener()
  val r2 = RelNoreferrer()
  val ok1 = case+ r1 of | RelNoopener() => true | _ => false
  val ok2 = case+ r2 of | RelNoreferrer() => true | _ => false
in ok1 && ok2 end

fn test_form_enctype(): bool = let
  val e1 = EnctypeUrlencoded()
  val e2 = EnctypeMultipart()
  val e3 = EnctypePlain()
  val ok1 = case+ e1 of | EnctypeUrlencoded() => true | _ => false
  val ok2 = case+ e2 of | EnctypeMultipart() => true | _ => false
  val ok3 = case+ e3 of | EnctypePlain() => true | _ => false
in ok1 && ok2 && ok3 end

fn test_input_types(): bool = let
  val t1 = InputText()
  val t2 = InputCheckbox()
  val t3 = InputHidden()
  val ok1 = case+ t1 of | InputText() => true | _ => false
  val ok2 = case+ t2 of | InputCheckbox() => true | _ => false
  val ok3 = case+ t3 of | InputHidden() => true | _ => false
in ok1 && ok2 && ok3 end

fn test_html_normal(): bool = let
  val d = Div()
  val s = Span()
  val h = H1()
  val p = P()
  val ok1 = case+ d of | Div() => true | _ => false
  val ok2 = case+ s of | Span() => true | _ => false
  val ok3 = case+ h of | H1() => true | _ => false
  val ok4 = case+ p of | P() => true | _ => false
in ok1 && ok2 && ok3 && ok4 end

fn test_html_normal_with_args(): bool = let
  val a = A("https://example.com", 1)
  val btn = Button(ButtonSubmit())
  val fm = Form("/submit", FormPost(), EnctypeUrlencoded())
  val ok1 = case+ a of | A(href, tgt) => $AR.eq_int_int(tgt, 1) | _ => false
  val ok2 = case+ btn of | Button(bt) => (case+ bt of | ButtonSubmit() => true | _ => false) | _ => false
  val ok3 = case+ fm of | Form(_, m, _) => (case+ m of | FormPost() => true | _ => false) | _ => false
in ok1 && ok2 && ok3 end

fn test_html_void(): bool = let
  val b = Br()
  val h = Hr()
  val i = HtmlInput(InputEmail(), "email", 0)
  val ok1 = case+ b of | Br() => true | _ => false
  val ok2 = case+ h of | Hr() => true | _ => false
  val ok3 = case+ i of | HtmlInput(t, _, _) => (case+ t of | InputEmail() => true | _ => false) | _ => false
in ok1 && ok2 && ok3 end

fn test_html_top(): bool = let
  val n = Normal(Div())
  val v = Void(Br())
  val ok1 = case+ n of | Normal(_) => true | _ => false
  val ok2 = case+ v of | Void(_) => true | _ => false
in ok1 && ok2 end

fn test_widget_id(): bool = let
  val r = Root()
  val ok1 = case+ r of | Root() => true | _ => false
in ok1 end

fn test_widget(): bool = let
  val t = Text("hello")
  val e = Element(Root(), Normal(Div()), 0, ~1)
  val ok1 = case+ t of | Text(_) => true | _ => false
  val ok2 = case+ e of | Element(_, _, h, _) => $AR.eq_int_int(h, 0) | _ => false
in ok1 && ok2 end

fn test_diff_remove_all(): bool = let
  val d = RemoveAllChildren(Root())
  val ok = case+ d of | RemoveAllChildren(id) => (case+ id of | Root() => true | _ => false) | _ => false
in ok end

fn test_diff_add_child(): bool = let
  val child = Text("world")
  val d = AddChild(Root(), child)
  val ok = case+ d of | AddChild(pid, _) => (case+ pid of | Root() => true | _ => false) | _ => false
in ok end

fn test_diff_set_hidden(): bool = let
  val d = SetHidden(Root(), 1)
  val ok = case+ d of | SetHidden(_, h) => $AR.eq_int_int(h, 1) | _ => false
in ok end

fn test_attribute_change(): bool = let
  val a1 = SetHref("https://example.com")
  val a2 = SetButtonType(ButtonReset())
  val a3 = SetFormMethod(FormGet())
  val a4 = SetImgLoading(LoadingLazy())
  val a5 = SetInputType(InputPassword())
  val a6 = SetDetailsOpen(1)
  val ok1 = case+ a1 of | SetHref(_) => true | _ => false
  val ok2 = case+ a2 of | SetButtonType(bt) => (case+ bt of | ButtonReset() => true | _ => false) | _ => false
  val ok3 = case+ a3 of | SetFormMethod(m) => (case+ m of | FormGet() => true | _ => false) | _ => false
  val ok4 = case+ a4 of | SetImgLoading(l) => (case+ l of | LoadingLazy() => true | _ => false) | _ => false
  val ok5 = case+ a5 of | SetInputType(t) => (case+ t of | InputPassword() => true | _ => false) | _ => false
  val ok6 = case+ a6 of | SetDetailsOpen(v) => $AR.eq_int_int(v, 1) | _ => false
in ok1 && ok2 && ok3 && ok4 && ok5 && ok6 end

fn test_th_scope(): bool = let
  val s1 = ScopeCol()
  val s2 = ScopeRow()
  val th = Th(2, 3, 1)
  val ok1 = case+ s1 of | ScopeCol() => true | _ => false
  val ok2 = case+ s2 of | ScopeRow() => true | _ => false
  val ok3 = case+ th of | Th(cs, rs, _) => $AR.eq_int_int(cs, 2) && $AR.eq_int_int(rs, 3) | _ => false
in ok1 && ok2 && ok3 end

fn test_link_target(): bool = let
  val t1 = Blank()
  val t2 = Self_()
  val t3 = Top_()
  val ok1 = case+ t1 of | Blank() => true | _ => false
  val ok2 = case+ t2 of | Self_() => true | _ => false
  val ok3 = case+ t3 of | Top_() => true | _ => false
in ok1 && ok2 && ok3 end

fn test_track_kind(): bool = let
  val k1 = TrackSubtitles()
  val k2 = TrackCaptions()
  val k3 = TrackMetadata()
  val ok1 = case+ k1 of | TrackSubtitles() => true | _ => false
  val ok2 = case+ k2 of | TrackCaptions() => true | _ => false
  val ok3 = case+ k3 of | TrackMetadata() => true | _ => false
in ok1 && ok2 && ok3 end

fn test_ol_list_type(): bool = let
  val o1 = OlDecimal()
  val o2 = OlLowerRoman()
  val ok1 = case+ o1 of | OlDecimal() => true | _ => false
  val ok2 = case+ o2 of | OlLowerRoman() => true | _ => false
in ok1 && ok2 end

fn test_mime_main(): bool = let
  val m1 = MimeText()
  val m2 = MimeImage()
  val m3 = MimeApplication()
  val ok1 = case+ m1 of | MimeText() => true | _ => false
  val ok2 = case+ m2 of | MimeImage() => true | _ => false
  val ok3 = case+ m3 of | MimeApplication() => true | _ => false
in ok1 && ok2 && ok3 end

end
