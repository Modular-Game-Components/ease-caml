type circle =
{
  pos: Raylib.Vector2.t ref;
  r: float;
}

module Double = struct
  open Raylib
  type v = Raylib.Vector2.t
  let ( *. ) = Fun.flip Vector2.scale
  let ( +. ) = Vector2.add
end

module V2Tween = Tween.Make(Double)

let ball : circle = { r = 40.0; pos = ref @@ Raylib.Vector2.create 200.0 150.0 }
let move = V2Tween.make_tween ball.pos (Raylib.Vector2.create 600.0 300.0) ~ef:Easers.quad 1.0
let tm = V2Tween.new_manager ()

let setup () =
  Raylib.init_window 800 450 "simple_tween";
  Raylib.set_target_fps 60;
  V2Tween.add move tm

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    V2Tween.update tm (Raylib.get_frame_time ());
    begin_drawing ();
    clear_background Color.raywhite;
    draw_circle_v !(ball.pos) ball.r Color.maroon;
    end_drawing ();
    loop ()

let () = setup () |> loop
