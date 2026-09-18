module Vector = struct
  type v = float
  let ( *. ) = ( *. )
  let ( +. ) = ( +. )
end

module FloatTween = Tween.Make(Vector)

type circle =
{
  r: float;
  x: float ref;
  y: float;
}

let ball : circle = { r = 40.0; x = ref 200.0; y = 225.0 }
let left = FloatTween.make_tween ball.x 600.0 ~ef:Easers.quad 1.0
let right = FloatTween.make_tween ball.x ~sv:600.0 200.0 ~ef:Easers.quad 1.0
let repeat = FloatTween.repeat (FloatTween.combine [left; right]) ~-1
let tm = FloatTween.new_manager ()

let setup () =
  Raylib.init_window 800 450 "simple_tween";
  Raylib.set_target_fps 60;
  FloatTween.add repeat tm

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    FloatTween.update tm (get_frame_time ());
    begin_drawing ();
    clear_background Color.raywhite;
    draw_circle_v (Vector2.create !(ball.x) ball.y) ball.r Color.maroon;
    end_drawing ();
    loop ()

let () = setup () |> loop
