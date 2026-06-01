type tween_node =
{
  start_val: float;
  end_val: float;
  ease_func: float -> float;
  repeat: int;
  mutable cur_repeat: int;
  mutable progress: float;
  obj: float ref;
}

type flat_tween =
{
  mutable index : int;
  repeat : int;
  mutable cur_repeat : int;
  tweens: tween_node array;
}

type tween = Flat of flat_tween 
           | Nested of {
             tweens: tween array;
             mutable index: int;
             repeat: int;
             mutable cur_repeat : int
           } 

type tween_manager = tween list ref

let make_tween_node (sv: float) (ev: float) (ef: float -> float) (obj: float ref) = {
  start_val = sv;
  end_val = ev;
  progress = 0.0;
  repeat = 1;
  cur_repeat = 0;
  ease_func = ef;
  obj = obj;
}

let make_tween (sv: float) (ev: float) (ef: float -> float) (obj: float ref) : tween =
  let tween_node = make_tween_node sv ev ef obj in 
  Flat {
    index = 0;
    repeat = 1;
    cur_repeat = 0;
    tweens = [|tween_node|];
  }

let repeat (t: tween) (count: int) = match t with
  | Flat t -> Flat {index = 0; repeat = count; cur_repeat = 0; tweens = t.tweens }
  | Nested t -> Nested {index = 0; repeat = count; cur_repeat = 0; tweens = t.tweens}

let update_node (node: tween_node) (dt: float) : unit =
  let p = node.ease_func (node.progress +. dt) in
  let sv = node.start_val in
  let ev = node.end_val in
  node.progress <- node.progress +. dt;
  node.obj := (1.0 -. p) *. sv +. p *. ev

let node_finished (tn: tween_node) = tn.progress >= 1.0

let reset_node (tn: tween_node) : unit = tn.progress <- 0.0
let rec reset_tween (t: tween) = match t with
  | Flat t -> t.cur_repeat <- 0; t.index <- 0; Array.iter reset_node t.tweens
  | Nested t -> t.cur_repeat <- 0; t.index <- 0; Array.iter reset_tween t.tweens

let should_restart_flat (t: flat_tween) : bool = 
    t.index = Array.length t.tweens - 1 
 && node_finished t.tweens.(t.index) 
 && (t.cur_repeat < t.repeat - 1 || t.repeat = ~-1)

let flat_tween_finished (t: flat_tween) =
    t.index = Array.length t.tweens - 1
 && node_finished t.tweens.(t.index)
 && not (should_restart_flat t)

let rec should_restart (t: tween) : bool = match t with
  | Flat t -> should_restart_flat t
  | Nested t -> (t.cur_repeat < t.repeat || t.repeat = ~-1)
              && t.index >= Array.length t.tweens - 1
              && is_finished t.tweens.(t.index)
and is_finished (t: tween) : bool = match t with
  | Flat t -> flat_tween_finished t
  | Nested t -> (t.cur_repeat = t.repeat && t.repeat <> ~-1) 
              && t.index >= Array.length t.tweens - 1 
              && is_finished t.tweens.(t.index)

let rec update_flat_tween (t: flat_tween) (dt: float) : unit =
  if not (node_finished t.tweens.(t.index)) then update_node t.tweens.(t.index) dt
  else if t.index < Array.length t.tweens - 1 then begin
    t.index <- t.index + 1;
    update_flat_tween t dt
  end
  else if should_restart_flat t then begin
    t.index <- 0;
    t.cur_repeat <- t.cur_repeat + 1;
    Array.iter reset_node t.tweens
  end

let rec update_tween (t: tween) (dt: float) : unit = match t with
  | Flat t -> update_flat_tween t dt
  | Nested t -> if not (is_finished t.tweens.(t.index)) then update_tween t.tweens.(t.index) dt
  else if should_restart (Nested t) then begin 
    t.index <- 0;
    Array.iter reset_tween t.tweens;
    t.cur_repeat <- t.cur_repeat + 1
  end


let update (tm: tween_manager) (dt: float) : unit =
  List.iter (fun x -> update_tween x dt) !tm;
  tm := List.filter (fun x -> not (is_finished x)) !tm

let new_manager () : tween_manager = ref []

let add (t: tween) (tm: tween_manager) : unit =
  tm := !tm @ [t]
