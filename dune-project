(lang dune 3.23)

(name ease-caml)

(generate_opam_files true)

(source
 (github Modular-Game-Components/ease-caml))

(authors "Christopher Sumnicht <csumnicht@berkeley.edu>")

(maintainers "Christopher Sumnicht <csumnicht@berkeley.edu>")

(license LICENSE)

(documentation https://modular-game-components.github.io/ease-caml/)

(package
 (name ease-caml)
 (synopsis "A easing/tweening library for OCaml.")
 (description "A tween transitions a float from one value to another over time using an easing function. This is useful for creating animations in games. ease-caml provides game framework agnostic methods to easily create tweens and manage them.")
 (depends ocaml)
 (tags
  ("game" "easing" "tween" "animation")))
