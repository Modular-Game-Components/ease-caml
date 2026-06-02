(** Datatypes and functions used to make tweens. Tweens are used to continuously
    change one float value to another float value over time. See 
    [examples/simple_tween.ml] for a simple example of how this is done in 
    conjunction with the Raylib game library. For common continuous functions 
    used to change the values over time, see the [Easers]. *)

(** The fundamental `tween` type. *)
type tween

(** Creates a tween. Takes a start value, end value, an easing function, a 
    duration also a reference to a value that will be ultimately changed by the 
    tween. *)
val make_tween : float ref -> ?sv:float -> float -> ?ef:(float -> float) -> float -> tween


(** A `tween_manager` is in charge of updating a collection of tweens in a game
    loop. See `example/simple_tween.ml` for how the tween_manager is used in the
    Raylib game loop. *)
type tween_manager = tween list ref

(** Create a new tween manager *)
val new_manager : unit -> tween_manager

(** Adds a tween to a particular tween_manager. *)
val add : tween -> tween_manager -> unit

(** Updates the values of the tweens that a particular tween_manager manages. *)
val update : tween_manager -> float -> unit

(** Takes a tween and creates a new tween that repeats the contents of the 
    original tween a number of times. If the number supplied is `~-1`, then
    the tween generated will repeat the original tween indefinitely. *)
val repeat : tween -> int -> tween

(** Take a tween and return a tween that plays the first tween *then* the
    second. *)
val extend : tween -> tween -> tween

(** Take a list of tweens and return a tween that plays each supplied tween in
    order. *)
val combine : tween list -> tween

(** Shorthand binary operation for `extend` *)
val ( $> ) : tween -> tween -> tween

(** Set the callback function for a tween. That is a function that is called
    after the tween finishes execution. *)
val set_callback : tween -> (unit -> unit) -> unit

(** Shorthand binary operation for `set_callback` *)
val ( $+ ) : tween -> (unit -> unit) -> unit
