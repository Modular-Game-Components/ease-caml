type circle =
{
  r: float;
  x: float ref;
  y: float;
}

let ball : circle = { r = 40.0; x = ref 200.0; y = 225.0 }
let left = Tween.make_tween 200.0 600.0 Easers.quad ball.x
let right = Tween.make_tween 600.0 200.0 Easers.quad ball.x
let repeat = Tween.repeat (Tween.combine [left; right]) ~-1
let tm = Tween.new_manager ()

let setup () =
  Raylib.init_window 800 450 "simple_tween";
  Raylib.set_target_fps 60;
  Tween.add repeat tm

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    Tween.update tm (get_frame_time ());
    begin_drawing ();
    clear_background Color.raywhite;
    draw_circle_v (Vector2.create !(ball.x) ball.y) ball.r Color.maroon;
    end_drawing ();
    loop ()

let () = setup () |> loop
