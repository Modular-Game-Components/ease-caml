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
  x: float ref;
  y: float;
}

let ball : circle = { r = 40.0; x = ref 200.0; y = 225.0 }
let left = FT.make_tween ball.x 600.0 ~ef:Easers.quad 1.0
let right = FT.make_tween ball.x ~sv:600.0 200.0 ~ef:Easers.quad 1.0
let repeat = FT.repeat (FT.extends left right) 2
let tm = FT.new_manager ()
let frames_dir = "frames"

let setup () =
  Raylib.init_window 800 450 "simple_tween";
  (* Screenshots log a message per frame at the default log level. *)
  Raylib.set_trace_log_level Raylib.TraceLogLevel.Error;
  Raylib.set_target_fps 60;
  FT.add repeat tm

let rec loop frame =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    FT.update tm (Raylib.get_frame_time ());
    begin_drawing ();
    clear_background Color.raywhite;
    draw_circle (Int.of_float !(ball.x)) (Int.of_float ball.y) ball.r Color.maroon;
    end_drawing ();
    (* raylib defers the actual capture to the end of the frame, so this grabs
       exactly one PNG per rendered frame. *)
    take_screenshot (Printf.sprintf "%s/frame_%04d.png" frames_dir frame);
    if (not (FT.running tm)) || window_should_close () then close_window ()
    else loop (frame + 1)

let () = setup (); loop 0
