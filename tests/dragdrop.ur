(*

drag and drop example

*)

(* styling *)

fun cssvalue (x: string) (y: string): css_property = value (property x) (atom y)
fun props_update (s: css_style) (xs : list (string * string)) : css_style =
    case xs of
    | [] => s
    | x :: xs => props_update (oneProperty s (cssvalue x.1 x.2)) xs
fun props (xs : list (string * string)): css_style = props_update noStyle xs

val div_style =
    props (("background-color", "#FF0000") :: ("width", "40px") :: ("height", "40px") :: [])

(* the deal *)

(*

in html5, you make an element draggable by setting attribute
[draggable=true], then implement [ondragstart : dragevent ->
transaction unit]

in [ondragstart], you set data transfer object values (it's a map from
MIME types(strings) to some values) --this is "what" is dragged

also, you set effectAllowed and dropEffect

then you make another element that listens to ondragenter/ondragover
events (this is where stuff can be dropped in) and call
preventDefault() in the handler if you want to allow the drop to
happen; you generally check the MIME type in the data transfer object
to decide

ondragleave: this is fired when the dragged element is leaving the
drop zone

if ondragenter/ondragover didn't cancel the drop, and the user
unpressed the mouse button over a drop zone, then the drop event
will be invoked on the drop zone element

also, a dragend event will be fired at the draggable element in two
cases: either the drop was cancelled, or it finished successfully; and
one can discriminate the two cases by the value of dropEffect property
(where "none" means that drop did not finish successfully)

draggable: dragstart, dragend
droppable: dragenter, dragover, dragleave, drop

draggable and droppable communicate via a source, and they should both
know the type
- dataTransfer : source (option a) (* some shared state *)
- dragstart : {} -> transaction a (* set the data transfer object to Some *)
- type drageffect = failure | drop
- dragend : drageffect -> transaction {}
- dragover / dragenter : a -> transaction drageffect (* decide what to do now *)
- drop : a -> transaction {} (* finally we can drop the data somewhere! *)

one issue here: the type a can't be global! it should be somehow shared by
dropzones and draggables

type dragResult = cancel | allow

functor Make(M : sig
                 type t (* what is being dragged *)

                 val dragStart : {} -> transaction t
                 val dragEnd : dragResult -> transaction {}
                 val dragEnter : t -> transaction dragResult
                 val dragOver : t -> transaction dragResult
                 val drop : t -> transaction {}

                 val gui_t : Gui.gui t body'
             end) : sig
  val create : {} -> transaction {}
end
 *)

(*
some more incomprehensible mumbling ahead.

we need:
- activation event (start of drag), which selects element 'a' as being dragged (and deselects whatever is being dragged)
  - we need access to element 'a' onmousedown handler for this
- deactivation event (end of drag), which we may want to use for dropping element 'a'
  - we need access to element 'a' onmouseup handler for this
- movement event (dragging), which affects the position of element 'a'
  - this is the source of "position" events

state:
- the element being dragged, or none
- if an element is being dragged, then we have to track mouse start position

type t (*draggable element*)
val position : t -> (int * int) -> transaction {} (*set left/top offset*)
val activate : t -> (mouseEvent -> transaction {}) -> transaction {} (*start of drag*)
val deactivate : t -> (mouseEvent -> transaction {}) -> transaction {} (*end of drag*)

now, the client must implement the signature above, and our drag/drop module
will do the following:
- call [position] on the right element when it is being dragged
- call [activate] to select the element which is to be dragged
- call [deactivate] to deselect the previously-dragged element

this is a typical OOP interface! another option is to "reverse" responsibilities.
we just provide a kind of container which can be dragged, embedding whatever
user wants to pass in. this "proxy" is what the module will work with.

the user will certainly want to react to dragging events, though.
so they may want to be notified of activation/deactivation of an element

SimpleEventA : eventkind -> CpsA target event // target -> event
  given a target element, install the handler onto it;
  wait for the event, and when it happens, yield it
handler : CpsA event target // event -> target
  called when [event] has happened, this will mutate the target in some way,
  then yield it (after that, the event handler is removed)
runA (nextA (SimpleEventA OnClick) handler) elem : transaction {}
  an example expression: wait for OnClick on some element, run the handler against it, asynchronously

combinators? then, or, either-or, loop

*)

fun draggable_div (x: xbody): transaction xbody =
  dragging <- source (None : option (int * int) (* mouse coordinates at drag start *));
  ofsCoords <- source (0,0); (* offset *)
  st <- source div_style;
  let
    fun dwn ev = set dragging (Some (ev.ScreenX, ev.ScreenY))
    fun up ev = set dragging None
    (* FIXME: it seems that [mv] should be called on the body of the page, likewise [up]? *)
    fun mv ev =
	drag <- get dragging;
	case drag of
	    None => return ()
	  | Some start =>
            ofs <- get ofsCoords;
            let
		(* NOTE: may apply another function here for
		 * horizontal/vertical sliders or to limit
		 * movement to some area/line
		 *)
		val coords' = (ofs.1 + (ev.ScreenX - start.1), ofs.2 + (ev.ScreenY - start.2))
            in
		set ofsCoords coords';
		set st
		    (props_update
			 div_style
                         (("position", "relative")
			      :: ("left", show coords'.1 ^ "px")
                              :: ("top", show coords'.2 ^ "px") :: []));
		set dragging (Some (ev.ScreenX, ev.ScreenY))
          end
  in
      (*
       FIXME: we should only need one event handler for mousemove/mouseup, since
       only one element can be dragged at a time, but we don't have access to elements...
       *)
      return <xml>
	<active code={onMousemove mv; onMouseup up; return <xml/>}></active>
	<div onmousedown={dwn} onmouseup={up} dynStyle={signal st}>{x}</div>
      </xml>
  end

fun main () : transaction page =
  d1 <- draggable_div <xml>Drag me!</xml>;
  d2 <- draggable_div <xml>Drag me too</xml>;
  return <xml><body>
    <p>this space stands for a wall of text</p>
    {d1}{d2}
  </body></xml>
