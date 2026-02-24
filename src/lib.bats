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
