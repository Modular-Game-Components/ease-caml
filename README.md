# ease-caml

A generic library for easing and tweening. A tween takes a floating point value from one value continuously over time to another value via an easing function.

# Example

Consider the following [Raylib Ocaml example](https://github.com/tjammer/raylib-ocaml) that draws a blank screen:

```ocaml
let setup () =
  Raylib.init_window 800 450 "raylib [core] example - basic window";
  Raylib.set_target_fps 60

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else (
    let open Raylib in
    begin_drawing ();
    clear_background Color.raywhite;
    end_drawing ();
    loop ()
  )

let () = setup () |> loop
```

We will adapt this to have a ball bounce on the screen. First we create a ball just outside the screen (add this to the *top* of the example):

```ocaml
type circle =
{
  r: float;
  x: float;
  y: float ref;
}

let ball : circle = { r = 40.0; x = 400.0; y = ref ~-.40.0 }
```

This will create a ball just outside the viewport. Note that `y` is a `float ref` since we will be making the ball fall in the `y` direction (and thus `y` must also be mutable!).

Now we will create a tween to move the ball, right below `let ball : circle = ...` add

```ocaml
let ty = Tween.make_tween ball.y 225.0 ~ef:Easers.bounce 1.0
```

`~-.40.0` is the start value of `y`, `225.0` is the end value and it will transition from start value to end value using a pre-defined `bounce` function for `1.0` duration. `ball.y` argument indicates what value `ty` is targeting.

Also, we will need a `tween_manager` that updates all of our tweens for us. Right below add:

```ocaml
let tm = Tween.new_manager ()
```

and in `setup` (anywhere really!) we need to add `ty` to the tween manager `tm` with:

```ocaml
Tween.add ty tm
```

`ty` will be in charge changing the `y` value. To make `ty` do work right below `let open Raylib in` add

```ocaml
Tween.update_tween ty (get_frame_time ());
```

Then after the `clear_background Color.raywhite;` add

```ocaml
draw_circle_v (Vector2.create ball.x !(ball.y)) ball.r Color.maroon;
```

to actually draw the ball. 

All in all you should now have:

```ocaml
type circle =
{
  r: float;
  x: float;
  y: float ref;
}

let ball : circle = { r = 40.0; x = 400.0; y = ref ~-.40.0 }
let ty = Tween.make_tween ball.y 225.0 ~ef:Easers.bounce 1.0
let tm = Tween.new_manager ()

let setup () =
  Raylib.init_window 800 450 "simple_tween";
  Raylib.set_target_fps 60;
  Tween.add ty tm

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    Tween.update_tween ty (get_frame_time ());
    begin_drawing ();
    clear_background Color.raywhite;
    draw_circle_v (Vector2.create ball.x !(ball.y)) ball.r Color.maroon;
    end_drawing ();
    loop ()

let () = setup () |> loop
```

This will make a red ball bounce on the screen. This is `example/simple_tween.ml`. It can be ran by going to the base directory and running:

```
dune build
dune exec -- ./examples/simple_tween.exe
```
