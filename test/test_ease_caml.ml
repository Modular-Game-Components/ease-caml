(* Test suite for ease-caml, written with alcotest.

   The library is [wrapped false], so its modules are available as [Tween]
   and [Easers] at the top level.

   A [tween] is internally an n-ary tree: the leaves ([Node]) are the
   actually-tweened values, and interiors ([Nested]) compose leaves via
   sequencing ([extends]/[combine]) and repetition ([repeat]). These tests
   exercise both levels through the public interface. *)

(* Rebind the infix operators so they can be used without opening the
   whole module. *)
let ( $> ) = Tween.( $> )
let ( $+ ) = Tween.( $+ )

(* ---------------------------------------------------------------------- *)
(* Helpers                                                                *)
(* ---------------------------------------------------------------------- *)

let approx_eps (eps : float) (a : float) (b : float) : bool =
  Float.abs (a -. b) <= eps

let approx = approx_eps 1e-9

let approx_float = Alcotest.testable Format.pp_print_float approx

(* [rough_float] is for invariants that only need to hold approximately,
   e.g. easing functions that end "near" 1.0. *)
let rough_float = Alcotest.testable Format.pp_print_float (approx_eps 1e-2)

let check_float msg expected actual =
  Alcotest.check approx_float msg expected actual

(* Runs a manager update and checks where the referenced value ended up. *)
let tick msg expected obj mgr dt =
  Tween.update mgr dt ;
  check_float msg expected !obj

(* Creates a fresh manager holding [tweens]. *)
let manager tweens =
  let tm = Tween.new_manager () in
  Tween.extend tweens tm ;
  tm

(* ---------------------------------------------------------------------- *)
(* Easers                                                                 *)
(* ---------------------------------------------------------------------- *)

let test_linear () =
  let f = Easers.linear in
  check_float "linear 0.0" 0.0 (f 0.0) ;
  check_float "linear 0.25" 0.25 (f 0.25) ;
  check_float "linear 0.5" 0.5 (f 0.5) ;
  check_float "linear 1.0" 1.0 (f 1.0) ;
  Alcotest.check rough_float "linear is the identity" 0.987654321 (f 0.987654321)

let test_quad () =
  let f = Easers.quad in
  check_float "quad 0.0" 0.0 (f 0.0) ;
  check_float "quad 0.5" 0.25 (f 0.5) ;
  check_float "quad 1.0" 1.0 (f 1.0)

let test_cubic () =
  let f = Easers.cubic in
  check_float "cubic 0.0" 0.0 (f 0.0) ;
  check_float "cubic 0.5" 0.125 (f 0.5) ;
  check_float "cubic 1.0" 1.0 (f 1.0)

let test_quart () =
  let f = Easers.quart in
  check_float "quart 0.0" 0.0 (f 0.0) ;
  check_float "quart 0.5" 0.0625 (f 0.5) ;
  check_float "quart 1.0" 1.0 (f 1.0)

let test_quint () =
  let f = Easers.quint in
  check_float "quint 0.0" 0.0 (f 0.0) ;
  check_float "quint 0.5" 0.03125 (f 0.5) ;
  check_float "quint 1.0" 1.0 (f 1.0)

let test_expo () =
  let f = Easers.expo in
  (* 0.0 is special-cased: 2 ** (10 *. 0.0 -. 10.0) would not be 0. *)
  check_float "expo 0.0" 0.0 (f 0.0) ;
  check_float "expo 0.5" 0.03125 (f 0.5) ;
  check_float "expo 1.0" 1.0 (f 1.0)

let test_circ () =
  let f = Easers.circ in
  check_float "circ 0.0" 0.0 (f 0.0) ;
  check_float "circ 0.5" (1.0 -. sqrt (0.75)) (f 0.5) ;
  check_float "circ 1.0" 1.0 (f 1.0)

let test_bounce () =
  let f = Easers.bounce in
  check_float "bounce 0.0" 0.0 (f 0.0) ;
  (* First parabolic arc: n *. x^2 for x < 1/2.5. *)
  check_float "bounce 0.2" 0.3025 (f 0.2) ;
  (* Bounces should land back near 1.0 at the end. *)
  Alcotest.check rough_float "bounce ends near 1.0" 1.0 (f 1.0) ;
  (* Bounce is not monotonic, but never undershoots 0. *)
  List.iter
    (fun x -> Alcotest.check rough_float "bounce is nonnegative" 0.0 (min 0.0 (f x)))
    [ 0.0; 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9; 1.0 ]

let easers_tests =
  [ ("linear", `Quick, test_linear) ;
    ("quad", `Quick, test_quad) ;
    ("cubic", `Quick, test_cubic) ;
    ("quart", `Quick, test_quart) ;
    ("quint", `Quick, test_quint) ;
    ("expo", `Quick, test_expo) ;
    ("circ", `Quick, test_circ) ;
    ("bounce", `Quick, test_bounce) ]

(* ---------------------------------------------------------------------- *)
(* Tween: basic leaves                                                    *)
(* ---------------------------------------------------------------------- *)

(* By default a tween starts at the current value of the reference. *)
let test_default_start_value () =
  let obj = ref 5.0 in
  ignore (Tween.make_tween obj 15.0 10.0) ;
  (* Creation alone must not modify the value. *)
  check_float "creation does not change obj" 5.0 !obj ;
  let tm = manager [ Tween.make_tween obj 15.0 10.0 ] in
  tick "halfway from the default start value" 10.0 obj tm 5.0

(* An explicit [~sv] overrides the current value of the reference. *)
let test_explicit_start_value () =
  let obj = ref 100.0 in
  let tm =
    manager [ Tween.make_tween obj ~sv:0.0 10.0 10.0 ]
  in
  tick "halfway between sv and ev" 5.0 obj tm 5.0

(* The default easing function is the identity. *)
let test_default_easing_is_linear () =
  let obj = ref 0.0 in
  let tm = manager [ Tween.make_tween obj 10.0 10.0 ] in
  tick "linear interpolation quarter" 2.5 obj tm 2.5 ;
  tick "linear interpolation half" 5.0 obj tm 2.5 ;
  tick "linear interpolation rest" 10.0 obj tm 5.0

(* A custom easing function reshapes the interpolation over time. *)
let test_custom_easing_function () =
  let obj = ref 0.0 in
  let tm =
    manager [ Tween.make_tween obj ~ef:Easers.quad 10.0 10.0 ]
  in
  (* progress = 0.5, eased = 0.25, value = 0.25 *. 10 = 2.5 *)
  tick "quad-eased halfway" 2.5 obj tm 5.0

(* A tween ends exactly on its end value, even if the update overshoots. *)
let test_tween_reaches_end_value () =
  let obj = ref 0.0 in
  let tm = manager [ Tween.make_tween obj 10.0 10.0 ] in
  tick "overshoot lands on end value" 10.0 obj tm 100.0 ;
  Alcotest.check Alcotest.bool "manager drops finished tween" false (Tween.running tm)

(* Callbacks fire when, and only when, a tween finishes. *)
let test_callback_on_finish () =
  let obj = ref 0.0 in
  let count = ref 0 in
  let t = Tween.make_tween obj 1.0 1.0 in
  Tween.set_callback t (fun () -> incr count) ;
  let tm = manager [ t ] in
  Tween.update tm 0.5 ;
  Alcotest.check Alcotest.int "callback not called before finishing" 0 !count ;
  tick "tween finished" 1.0 obj tm 0.5 ;
  Alcotest.check Alcotest.int "callback called exactly once" 1 !count ;
  (* Once finished the tween is out of the manager, so no more callbacks. *)
  Tween.update tm 1.0 ;
  Alcotest.check Alcotest.int "callback not called again" 1 !count

(* [( $+ )] is shorthand for [set_callback]. *)
let test_callback_operator () =
  let obj = ref 0.0 in
  let count = ref 0 in
  let t = Tween.make_tween obj 1.0 1.0 in
  t $+ (fun () -> incr count) ;
  let tm = manager [ t ] in
  tick "callback operator tween finished" 1.0 obj tm 1.0 ;
  Alcotest.check Alcotest.int "callback operator fired callback" 1 !count

let basic_tween_tests =
  [ ("default start value", `Quick, test_default_start_value) ;
    ("explicit start value", `Quick, test_explicit_start_value) ;
    ("default easing is linear", `Quick, test_default_easing_is_linear) ;
    ("custom easing function", `Quick, test_custom_easing_function) ;
    ("tween reaches end value", `Quick, test_tween_reaches_end_value) ;
    ("callback on finish", `Quick, test_callback_on_finish) ;
    ("callback operator", `Quick, test_callback_operator) ]

(* ---------------------------------------------------------------------- *)
(* Tween: composition                                                     *)
(* ---------------------------------------------------------------------- *)

(* [repeat 3] plays the tween three times, then the manager drops it. *)
let test_repeat_finite () =
  let obj = ref 0.0 in
  let t = Tween.repeat (Tween.make_tween obj 1.0 1.0) 3 in
  let tm = manager [ t ] in
  tick "cycle 1 halfway" 0.5 obj tm 0.5 ;
  tick "cycle 1 done" 1.0 obj tm 0.5 ;
  tick "cycle 2 halfway (value resets)" 0.5 obj tm 0.5 ;
  tick "cycle 2 done" 1.0 obj tm 0.5 ;
  tick "cycle 3 halfway" 0.5 obj tm 0.5 ;
  tick "cycle 3 done" 1.0 obj tm 0.5 ;
  Alcotest.check Alcotest.bool "repeated tween stops after last cycle" false
    (Tween.running tm) ;
  tick "stays at end value after stopping" 1.0 obj tm 1.0

(* [repeat ~-1] plays the tween indefinitely. *)
let test_repeat_forever () =
  let obj = ref 0.0 in
  let t = Tween.repeat (Tween.make_tween obj 1.0 1.0) (-1) in
  let tm = manager [ t ] in
  for _ = 1 to 10 do
    tick "infinite repeat halfway" 0.5 obj tm 0.5 ;
    tick "infinite repeat end" 1.0 obj tm 0.5
  done ;
  Alcotest.check Alcotest.bool "infinite repeat keeps running" true (Tween.running tm)

(* [extends]/[$>] plays the first tween, then the second. *)
let test_extends () =
  let a = ref 0.0 and b = ref 0.0 in
  let t1 = Tween.make_tween a ~sv:0.0 10.0 10.0 in
  let t2 = Tween.make_tween b ~sv:0.0 100.0 10.0 in
  let tm = manager [ t1 $> t2 ] in
  tick "first tween drives a, b untouched" 10.0 a tm 10.0 ;
  check_float "second tween has not started" 0.0 !b ;
  Alcotest.check Alcotest.bool "sequence still running" true (Tween.running tm) ;
  tick "second tween drives b, a frozen" 100.0 b tm 10.0 ;
  check_float "first tween stays at its end value" 10.0 !a ;
  Alcotest.check Alcotest.bool "sequence finished" false (Tween.running tm)

(* [extends] also composes nested (repeated) tweens. *)
let test_extends_repeated_tweens () =
  let a = ref 0.0 and b = ref 0.0 in
  let t1 = Tween.repeat (Tween.make_tween a ~sv:0.0 1.0 1.0) 2 in
  let t2 = Tween.make_tween b ~sv:0.0 10.0 1.0 in
  let tm = manager [ Tween.extends t1 t2 ] in
  tick "repeat: cycle 1 halfway" 0.5 a tm 0.5 ;
  tick "repeat: cycle 1 done" 1.0 a tm 0.5 ;
  tick "repeat: cycle 2 halfway" 0.5 a tm 0.5 ;
  tick "repeat: cycle 2 done" 1.0 a tm 0.5 ;
  (* One more update hands control from the repeated tween to its
     successor in the sequence. *)
  tick "sequence advances to second tween" 1.0 a tm 0.1 ;
  check_float "second tween not started yet" 0.0 !b ;
  tick "second tween finished" 10.0 b tm 1.0 ;
  Alcotest.check Alcotest.bool "combined sequence finished" false (Tween.running tm)

(* [combine] sequences a whole list of tweens. *)
let test_combine () =
  let a = ref 0.0 and b = ref 0.0 and c = ref 0.0 in
  let t1 = Tween.make_tween a ~sv:0.0 1.0 1.0 in
  let t2 = Tween.make_tween b ~sv:0.0 2.0 1.0 in
  let t3 = Tween.make_tween c ~sv:0.0 3.0 1.0 in
  let tm = manager [ Tween.combine [ t1 ; t2 ; t3 ] ] in
  tick "combine: first tween done" 1.0 a tm 1.0 ;
  check_float "combine: second not started" 0.0 !b ;
  tick "combine: second tween done" 2.0 b tm 1.0 ;
  check_float "combine: third not started" 0.0 !c ;
  tick "combine: third tween done" 3.0 c tm 1.0 ;
  Alcotest.check Alcotest.bool "combine: sequence finished" false (Tween.running tm)

(* [combine] of a single tween is just that tween. *)
let test_combine_single () =
  let obj = ref 0.0 in
  let tm = manager [ Tween.combine [ Tween.make_tween obj ~sv:0.0 4.0 2.0 ] ] in
  tick "single-tween combine halfway" 2.0 obj tm 1.0 ;
  tick "single-tween combine done" 4.0 obj tm 1.0

(* [( $> )] is shorthand for [extends] and chains left-associatively. *)
let test_extends_operator () =
  let a = ref 0.0 and b = ref 0.0 and c = ref 0.0 in
  let tm =
    manager
      [ Tween.make_tween a ~sv:0.0 1.0 1.0
        $> Tween.make_tween b ~sv:0.0 2.0 1.0
        $> Tween.make_tween c ~sv:0.0 3.0 1.0 ]
  in
  tick "$>: first done" 1.0 a tm 1.0 ;
  check_float "$>: second not started" 0.0 !b ;
  tick "$>: second done" 2.0 b tm 1.0 ;
  check_float "$>: third not started" 0.0 !c ;
  (* One more update hands control from the sequence (a $> b) to c. *)
  tick "$>: sequence advances to third tween" 2.0 b tm 0.1 ;
  tick "$>: third done" 3.0 c tm 1.0 ;
  Alcotest.check Alcotest.bool "$>: all finished" false (Tween.running tm)

let composition_tests =
  [ ("repeat finite", `Quick, test_repeat_finite) ;
    ("repeat forever", `Quick, test_repeat_forever) ;
    ("extends", `Quick, test_extends) ;
    ("extends repeated tweens", `Quick, test_extends_repeated_tweens) ;
    ("combine", `Quick, test_combine) ;
    ("combine single", `Quick, test_combine_single) ;
    ("extends operator", `Quick, test_extends_operator) ]

(* ---------------------------------------------------------------------- *)
(* Tween: manager                                                         *)
(* ---------------------------------------------------------------------- *)

let test_new_manager () =
  let tm = Tween.new_manager () in
  Alcotest.check Alcotest.bool "fresh manager is not running" false (Tween.running tm)

let test_add () =
  let obj = ref 0.0 in
  let tm = Tween.new_manager () in
  Tween.add (Tween.make_tween obj ~sv:0.0 10.0 10.0) tm ;
  Alcotest.check Alcotest.bool "manager with a tween is running" true (Tween.running tm) ;
  tick "added tween updates" 5.0 obj tm 5.0

let test_extend () =
  let a = ref 0.0 and b = ref 0.0 in
  let tm =
    manager
      [ Tween.make_tween a ~sv:0.0 10.0 10.0 ;
        Tween.make_tween b ~sv:0.0 100.0 10.0 ]
  in
  tick "first tween in batch updated" 5.0 a tm 5.0 ;
  check_float "second tween in batch updated" 50.0 !b

(* A manager drops individual tweens as they finish, keeping the rest. *)
let test_manager_removes_finished_tweens () =
  let a = ref 0.0 and b = ref 0.0 in
  let tm =
    manager
      [ Tween.make_tween a ~sv:0.0 10.0 1.0 ;
        Tween.make_tween b ~sv:0.0 10.0 10.0 ]
  in
  tick "short tween done" 10.0 a tm 1.0 ;
  check_float "long tween only partway" 1.0 !b ;
  Alcotest.check Alcotest.bool "long tween still running" true (Tween.running tm) ;
  tick "long tween done" 10.0 b tm 9.0 ;
  Alcotest.check Alcotest.bool "manager empty once all done" false (Tween.running tm)

(* Eased tweens are driven through the manager too. *)
let test_manager_with_eased_tween () =
  let obj = ref 0.0 in
  let tm =
    manager [ Tween.make_tween obj ~ef:Easers.cubic 8.0 2.0 ]
  in
  (* progress = 0.5, eased = 0.125, value = 8 *. 0.125 *)
  tick "eased tween through manager" 1.0 obj tm 1.0

let manager_tests =
  [ ("new manager", `Quick, test_new_manager) ;
    ("add", `Quick, test_add) ;
    ("extend", `Quick, test_extend) ;
    ("manager removes finished tweens", `Quick, test_manager_removes_finished_tweens) ;
    ("manager with eased tween", `Quick, test_manager_with_eased_tween) ]

(* ---------------------------------------------------------------------- *)
(* Runner                                                                 *)
(* ---------------------------------------------------------------------- *)

let () =
  Alcotest.run "ease_caml"
    [ ("easers", easers_tests) ;
      ("basic tweens", basic_tween_tests) ;
      ("tween composition", composition_tests) ;
      ("tween manager", manager_tests) ]
