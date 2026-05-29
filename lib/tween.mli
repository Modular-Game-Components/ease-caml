type tween_node =
{
  start_val: float;
  end_val: float;
  ease_func: float -> float;
  mutable progress: float;
  obj: float ref;
}

type tween

type tween_manager = tween list ref

val make_tween : float -> float -> (float -> float) -> float ref -> tween
val update_tween : tween -> float -> unit
val update : tween_manager -> float -> unit
val new_manager : unit -> tween_manager
val add : tween -> tween_manager -> unit
