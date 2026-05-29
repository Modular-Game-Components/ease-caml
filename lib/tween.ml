type tween_node =
{
  start_val: float;
  end_val: float;
  ease_func : float -> float;
  mutable progress: float;
  obj: float ref;
}

type tween = 
{
  mutable index : int;
  nodes : tween_node array;
}

let node_done (tn: tween_node) =
  if tn.progress > 1.0 then true else false

type tween_manager = tween list ref

let make_tween_node (sv: float) (ev: float) (ef: float -> float) (obj: float ref) = {
  start_val = sv;
  end_val = ev;
  progress = 0.0;
  ease_func = ef;
  obj = obj;
}

let make_tween (sv: float) (ev: float) (ef: float -> float) (obj: float ref) =
  let tween_node = make_tween_node sv ev ef obj in
  {
    index = 0;
    nodes = [|tween_node|];
  }

let update_node (node: tween_node) (dt: float) : unit =
  let p = node.ease_func (node.progress +. dt) in
  let sv = node.start_val in
  let ev = node.end_val in
  node.progress <- node.progress +. dt;
  node.obj := (1.0 -. p) *. sv +. p *. ev

let node_finished (tn: tween_node) = tn.progress >= 1.0

let finished (t: tween) = function
  | [] -> true
  | _  -> false

let update_tween (t: tween) (dt: float) : unit = if t.index = Array.length t.nodes 
  then () else match t.nodes with
  | [||] -> ()
  | _ -> match node_finished t.nodes.(t.index) with
    | true  -> t.index <- t.index + 1
    | false -> update_node t.nodes.(0) dt

let tween_finished (t: tween) = if t.index = Array.length t.nodes then true else false

let update (tm: tween_manager) (dt: float) : unit =
  List.iter (fun x -> update_tween x dt) !tm;
  tm := List.filter (fun x -> not (tween_finished x)) !tm

let new_manager () : tween_manager = ref []

let add (t: tween) (tm: tween_manager) : unit =
  tm := !tm @ [t]
