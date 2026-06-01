type circle =
{
  r: float;
  x: float;
  y: float ref;
}

let ball : circle = { r = 40.0; x = 400.0; y = ref (0.0 -. 40.0) }
let ty = Tween.make_tween (0.0 -. 40.0) 225.0 Easers.bounce ball.y
let tm = Tween.new_manager ()

let setup () =
  Raylib.init_window 800 450 "simple_tween";
  Raylib.set_target_fps 60;
  Tween.add ty tm

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else (
    let open Raylib in
    Tween.update tm (get_frame_time ());
    begin_drawing ();
    clear_background Color.raywhite;
    draw_circle_v (Vector2.create ball.x !(ball.y)) ball.r Color.maroon;
    end_drawing ();
    loop ()
  )

let () = setup () |> loop
