type tween_node =
{
  start_val: float;
  end_val: float;
  ease_func: float -> float;
  mutable progress: float;
  repeat: int;
  mutable cur_repeat: int;
  duration: float;
  obj: float ref;
}

type tween = Node of tween_node
           | Nested of {
             tween_left: tween;
             tween_right: tween;
             repeat: int;
             mutable cur_repeat : int
           } 

type tween_manager = tween list ref

let make_tween_node (obj: float ref) ?(sv: float = !obj) (ev: float) ?(ef: float -> float = (fun x -> x)) (d: float) = {
  start_val = sv;
  end_val = ev;
  progress = 0.0;
  ease_func = ef;
  obj = obj;
  repeat = 1;
  cur_repeat = 0;
  duration = d;
}

let make_tween (obj: float ref) ?(sv: float = !obj) (ev: float) ?(ef: float -> float = (fun x -> x)) (d: float) : tween =
  let tween_node = make_tween_node obj ~sv:sv ev ~ef:ef d in 
  Node tween_node

let repeat (t: tween) (count: int) = match t with
  | Node t -> Node {
                repeat = count;
                cur_repeat = 0;
                start_val = t.start_val;
                end_val = t.end_val;
                ease_func = t.ease_func;
                progress = 0.0;
                duration = t.duration;
                obj = t.obj;
              }
  | Nested t -> Nested {
                  repeat = count;
                  cur_repeat = 0;
                  tween_left = t.tween_left;
                  tween_right = t.tween_right;
                }

let should_restart_node (tn: tween_node) = 
        tn.progress >= 1.0 && (tn.cur_repeat < tn.repeat - 1 || tn.repeat = -1)
let node_finished (tn: tween_node) = tn.progress >= 1.0 
                                  && tn.cur_repeat = tn.repeat - 1

let update_node (node: tween_node) (dt: float) : unit =
  match should_restart_node node with
    | true -> node.progress <- 0.0; 
              node.cur_repeat <- node.cur_repeat + 1;
              node.obj := node.start_val
    | false ->  let dur = node.duration in
                let p = node.ease_func (node.progress +. (dt /. dur)) in
                let sv = node.start_val in
                let ev = node.end_val in 
                node.progress <- node.progress +. (dt /. dur);
                node.obj := (1.0 -. p) *. sv +. p *. ev

let rec reset_tween (t: tween) = match t with
  | Node t -> t.cur_repeat <- 0; 
              t.progress <- 0.0;
  | Nested t -> t.cur_repeat <- 0;
                reset_tween t.tween_left;
                reset_tween t.tween_right

let rec is_finished (t: tween) : bool = match t with
  | Node t -> node_finished t 
  | Nested t -> (t.cur_repeat = t.repeat && t.repeat <> ~-1) 
              && is_finished t.tween_right

let rec update_tween (t: tween) (dt: float) : unit = match t with
  | Node t -> update_node t dt
  | Nested t -> if not (is_finished t.tween_left) then
      update_tween t.tween_left dt
    else if not (is_finished t.tween_right) then
      update_tween t.tween_right dt
    else if (t.cur_repeat < t.repeat - 1 || t.repeat = -1) then begin
      t.cur_repeat <- t.cur_repeat + 1;
      reset_tween (Nested t)
    end

let extend (t1: tween) (t2: tween) =
  Nested {
    tween_left = t1;
    tween_right = t2;
    repeat = 1;
    cur_repeat = 0;
  }

let ( $> ) = extend

let dummy = ref 0.0
let empty_tween =
{
  start_val = 0.0;
  end_val = 0.0;
  ease_func = (fun x -> x);
  progress = 0.0;
  repeat = 1;
  cur_repeat = 0;
  duration = 0.0;
  obj = dummy;
} 

let rec combine (tweens: tween list) : tween = match tweens with
  | [] -> Node empty_tween
  | [a] -> a
  | h::t -> Nested {
              tween_left = h;
              tween_right = combine t;
              repeat = 1;
              cur_repeat = 0;
            }

let update (tm: tween_manager) (dt: float) : unit =
  List.iter (fun x -> update_tween x dt) !tm;
  tm := List.filter (fun x -> not (is_finished x)) !tm

let new_manager () : tween_manager = ref []

let add (t: tween) (tm: tween_manager) : unit =
  tm := !tm @ [t]
