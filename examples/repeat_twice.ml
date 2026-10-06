module FT = Tween.FloatTween

type circle =
{
  r: float;
  x: float;
  y: float ref;
}

let ball : circle = { r = 40.0; x = 400.0; y = ref ~-.40.0 }
let ty = FT.make_tween ball.y 225.0 ~ef:Easers.bounce 1.0
let tyr = FT.repeat ty 2
let tm = FT.new_manager ()

let setup () =
  Raylib.init_window 800 450 "simple_tween";
  Raylib.set_target_fps 60;
  FT.add tyr tm

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    FT.update tm (Raylib.get_frame_time ());
    begin_drawing ();
    clear_background Color.raywhite;
    draw_circle_v (Raylib.Vector2.create ball.x !(ball.y)) ball.r Color.maroon;
    end_drawing ();
    loop ()

let () = setup () |> loop
