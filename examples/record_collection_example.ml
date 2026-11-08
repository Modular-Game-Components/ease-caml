type circle =
{
  pos: Raylib.Vector2.t ref;
  r: float;
}

module Double = struct
  open Raylib
  type v = Raylib.Vector2.t
  let ( *. ) (s: float) (v: Vector2.t) = Vector2.create (s *. (Vector2.x v)) (s *. (Vector2.y v))
  let ( +. ) (v: Vector2.t) (w: Vector2.t) = Vector2.create (Vector2.x v +. Vector2.x w) (Vector2.y v +. Vector2.y w)
end

module V2Tween = Tween.Make(Double)

let ball : circle = { r = 40.0; pos = ref @@ Raylib.Vector2.create 200.0 150.0 }
let move = V2Tween.make_tween ball.pos (Raylib.Vector2.create 600.0 300.0) ~ef:Easers.quad 1.0
let tm = V2Tween.new_manager ()
let frames_dir = "frames"

let setup () =
  Raylib.init_window 800 450 "simple_tween";
  Raylib.set_target_fps 60;
  Raylib.set_trace_log_level Raylib.TraceLogLevel.Error;
  (try Sys.mkdir frames_dir 0o755 with Sys_error _ -> ());
  V2Tween.add move tm

let rec loop frame =
  let open Raylib in
  V2Tween.update tm (Raylib.get_frame_time ());
  begin_drawing ();
  clear_background Color.raywhite;
  draw_circle_v !(ball.pos) ball.r Color.maroon;
  end_drawing ();
  take_screenshot (Printf.sprintf "%s/frame_%04d.png" frames_dir frame);
  if (not (V2Tween.running tm)) || window_should_close () then close_window ()
  else loop (frame + 1)

let () = setup (); loop 0
