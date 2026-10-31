(* Records the simple_tween animation to one PNG per rendered frame in
   [frames/]. The window closes automatically once the tween finishes, so the
   capture is frame-exact. Afterwards, assemble a video or GIF with ffmpeg:

     ffmpeg -framerate 60 -i frames/frame_%04d.png -c:v libx264 \
       -pix_fmt yuv420p simple_tween.mp4

     ffmpeg -framerate 60 -i frames/frame_%04d.png \
       -vf "fps=15,scale=480:-1:flags=lanczos,split[a][b];[a]palettegen[p];[b][p]paletteuse" \
       simple_tween.gif *)

module FT = Tween.FloatTween

type circle =
{
  r: float;
  x: float;
  y: float ref;
}

let ball : circle = { r = 40.0; x = 400.0; y = ref (0.0 -. 40.0) }
let ty = FT.make_tween ball.y 225.0 ~ef:Easers.bounce 1.0
let tm = FT.new_manager ()
let frames_dir = "frames"

let setup () =
  Raylib.init_window 800 450 "record_simple_tween";
  Raylib.set_target_fps 60;
  (* Screenshots log a message per frame at the default log level. *)
  Raylib.set_trace_log_level Raylib.TraceLogLevel.Error;
  (try Sys.mkdir frames_dir 0o755 with Sys_error _ -> ());
  FT.add ty tm

let rec loop frame =
  FT.update tm (Raylib.get_frame_time ());
  let open Raylib in
  begin_drawing ();
  clear_background Color.raywhite;
  draw_circle_v (Vector2.create ball.x !(ball.y)) ball.r Color.maroon;
  end_drawing ();
  (* raylib defers the actual capture to the end of the frame, so this grabs
     exactly one PNG per rendered frame. *)
  take_screenshot (Printf.sprintf "%s/frame_%04d.png" frames_dir frame);
  if (not (FT.running tm)) || window_should_close () then close_window ()
  else loop (frame + 1)

let () = setup (); loop 0
