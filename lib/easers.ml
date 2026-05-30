let linear x = x
let quad x = x *. x
let cubic x = x ** 3.0
let quart x = x ** 4.0
let quint x = x ** 5.0
let expo x = match x with
  | 0.0 -> 0.0
  | _   -> 2.0 ** (10.0 *. x -. 10.0)
let circ x = 1.0 -. (1.0 -. x ** 2.0) ** 0.5
let back x =
  let c1 = 1.70158 in
  let c2 = c1 +. 1.0 in
  c2 *. (x ** 3.0) -. c1 *. (x ** 2.0)
let elastic (x : float) : float =
  let c = (2.0 *. Float.pi) /. 3.0 in
  match x with
  | 0.0 -> 0.0
  | 1.0 -> 1.0
  | _   -> Float.neg (2.0 ** (10.0 *. x -. 10.0)) *. Float.sin((x *. 10.0 -. 10.75) *. c)
let bounce x = 
  let n = 7.5625 in
  let d = 2.5 in
  let y0 = x -. 1.5 /. d in
  let y1 = x -. 2.25 /. d in
  let y2 = x -. 2.625 /. d in
  if x < 1.0 /. d then n *. x ** 2.0
  else if x < 2.0 /. d then n *. y0 ** 2.0 +. 0.75
  else if x < 2.5 /. d then n *. y1 ** 2.0 +. 0.9375
  else n *. y2 ** 2.0 +. 0.984375

let inout f =
  (fun x -> if x < 1.0 then 0.5 *. f (2.0 *. x) else (1.0 -. f (2.0 *. x)) +. 0.5)
let out f =
  (fun x -> (1 - f x))
