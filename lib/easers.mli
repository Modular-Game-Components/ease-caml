(** A collection of common easing functions for tweens adapted to OCaml from
    {{:https://easings.net/}easings.net} *)

val linear : float -> float
val quad : float -> float
val cubic : float -> float
val quart : float -> float
val quint : float -> float
val expo : float -> float
val circ : float -> float
val bounce : float -> float

(** {1 Combinators}

    Every easer above is an {i ease-in} curve: it starts slowly and accelerates.
    These combinators derive the other two standard shapes from any such curve,
    so there is no need for a separate function per variant. *)

(** [out f] mirrors the ease-in curve [f] into the corresponding {i ease-out}
    curve, which starts quickly and decelerates. Defined as
    [1. -. f (1. -. x)], so [out f] preserves the [f 0. = 0.] / [f 1. = 1.]
    contract, and [out linear] is [linear]. *)
val out : (float -> float) -> float -> float

(** [inout f] combines the ease-in curve [f] with its mirror into an
    {i ease-in-out} curve, which accelerates over the first half and
    decelerates over the second. It passes through [0.5] at [x = 0.5], and
    [inout linear] is [linear]. *)
val inout : (float -> float) -> float -> float
