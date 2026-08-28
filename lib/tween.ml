(** Utility functions for sequences *)

(* Repeat a sequence n times. If n = -1 then repeat forever. *)
let repeat_n (seq: 'a Seq.t) (n: int) : ('a Seq.t) = 
  match n with
  | (-1) -> Seq.concat (Seq.repeat seq)
  | _ -> Seq.take n (Seq.concat (Seq.repeat seq))

(* Like Seq.uncons but a little safer. *)
let safe_next (seq: 'a Seq.t): 'a option * 'a Seq.t =
  match Seq.uncons seq with
      | Some (h, t) -> (Some h, t)
      | None -> (None, Seq.empty)

(* A primitive tween *)
type tween_node =
{
  start_val: float;
  end_val: float;
  ease_func: float -> float;
  mutable progress: float;
  duration: float;
  obj: float ref;
  mutable callback : unit -> unit;
  mutable parent: tween_interior option;
}
(* A composition of tween leaves (or other tween interiors. *)
and tween_interior =
{
  children: tween list;
  repeat: int;
  mutable callback: unit -> unit;
  mutable seq: tween Seq.t;
  mutable cur: tween option;
  mutable parent: tween_interior option;
}
(* An ordered, rooted, n-ary tree. *)
and tween = Node of tween_node | Nested of tween_interior

(* Called in the update function and updates every tween added to it. *)
type tween_manager = tween list ref

(* Constructor for a tween. *)
let make_tween_node (obj: float ref) ?(sv: float = !obj) (ev: float) ?(ef: float -> float = (fun x -> x)) (d: float) = {
  start_val = sv;
  end_val = ev;
  progress = 0.0;
  ease_func = ef;
  obj = obj;
  duration = d;
  callback = (fun () -> ());
  parent = None;
}

let make_tween (obj: float ref) ?(sv: float = !obj) (ev: float) ?(ef: float -> float = (fun x -> x)) (d: float) : tween =
  let tween_node = make_tween_node obj ~sv:sv ev ~ef:ef d in 
  Node tween_node

let repeat (t: tween) (count: int) = match t with
  | Node tn -> let new_seq = repeat_n (List.to_seq [Node tn]) count in
               let head, tail = safe_next new_seq in
               let new_tween = {
                repeat = count;
                children = [t];
                callback = tn.callback;
                parent = tn.parent;
                seq = tail;
                cur = head;
              } in
              tn.parent <- Some new_tween;
              Nested new_tween
  | Nested t -> let new_seq = repeat_n (List.to_seq t.children) count in
                let head, tail = safe_next new_seq in
                let new_tween = {
                  repeat = count;
                  children = t.children;
                  callback = t.callback;
                  seq = tail;
                  cur = head;
                  parent = None;
                } in
                List.iter (fun c -> 
                  (match c with
                  | Node x -> x.parent <- Some new_tween
                  | Nested x -> x.parent <- Some new_tween)) t.children;
                Nested new_tween
  
let node_finished (tn: tween_node) = tn.progress >= 1.0

let update_leaf (node: tween_node) (dt: float) : unit =
  let dur = node.duration in
  let p = node.ease_func (node.progress +. (dt /. dur)) in
  let sv = node.start_val in
  let ev = node.end_val in
  node.progress <- node.progress +. (dt /. dur);
  node.obj := (1.0 -. p) *. sv +. p *. ev;
  if node_finished node then node.callback () else ()

let rec reset_tween (t: tween) = match t with
  | Node t -> t.progress <- 0.0
  | Nested t -> 
    let new_seq = repeat_n (List.to_seq t.children) t.repeat in
    let head, tail = safe_next new_seq in
    t.seq <- tail;
    t.cur <- head;
    List.iter reset_tween t.children

let rec is_finished (t: tween) : bool = match t with
  | Node t -> node_finished t 
  | Nested t -> t.cur = None

let rec update_cur_tween (t: tween option) (dt: float) : unit =
  match t with
  | Some (Node tn) -> if node_finished tn then
    (reset_tween (Node tn);
    match tn.parent with
    | Some p -> 
        let head, tail = safe_next p.seq in
        p.seq <- tail;
        p.cur <- head
    | None -> ())
    else (update_leaf tn dt)
  | Some (Nested tw) -> update_cur_tween tw.cur dt
  | None -> ()

let rec update_tween (t: tween) (dt: float) : unit = match t with
  | Node tw -> update_leaf tw dt
  | Nested tw -> update_cur_tween tw.cur dt

let extends (t1: tween) (t2: tween): tween =
  let new_seq = List.to_seq [t1 ; t2] in
  let head, tail = safe_next new_seq in
  let new_tween = {
    children = [t1 ; t2];
    repeat = 1;
    callback = (fun () -> ());
    seq = tail;
    cur = head;
    parent = None
  } in
  (match t1 with
  | Node t -> t.parent <- Some new_tween
  | Nested t -> t.parent <- Some new_tween);
  (match t2 with
  | Node t -> t.parent <- Some new_tween
  | Nested t -> t.parent <- Some new_tween);
  Nested new_tween

let ( $> ) = extends

let dummy = ref 0.0
let empty_tween =
{
  start_val = 0.0;
  end_val = 0.0;
  ease_func = (fun x -> x);
  progress = 0.0;
  duration = 0.0;
  obj = dummy;
  callback = (fun () -> ());
  parent = None;
} 

let rec combine (tweens: tween list) : tween = match tweens with
  | [] -> Node empty_tween
  | [a] -> a
  | _ -> let new_seq = List.to_seq tweens in
         let head, tail = safe_next new_seq in
         let new_tween = {
           children=tweens;
           repeat = 1;
           callback = (fun () -> ());
           seq = tail;
           cur = head;
           parent = None;
         } in
         List.iter (fun t -> 
           match t with 
           | Node x -> x.parent <- Some new_tween 
           | Nested x -> x.parent <- Some new_tween) tweens;
         Nested new_tween

let set_callback (t: tween) (f: unit -> unit) = match t with
  | Node t -> t.callback <- f
  | Nested t -> t.callback <- f

let ( $+ ) = set_callback

let update (tm: tween_manager) (dt: float) : unit =
  List.iter (fun x -> update_tween x dt) !tm;
  tm := List.filter (fun x -> not (is_finished x)) !tm


let new_manager () : tween_manager = ref []

let add (t: tween) (tm: tween_manager) : unit =
  tm := !tm @ [t]

let rec extend (tws: tween list) (tm: tween_manager) : unit =
  match tws with
  | [] -> ()
  | h :: t -> add h tm; extend t tm

let running (tm: tween_manager): bool =
  match !tm with
  | [] -> false
  | _ -> true
