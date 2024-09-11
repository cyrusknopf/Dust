(* 
    OCaml implemenation of a zipper inspired by this chapter:
    https://en.wikibooks.org/wiki/Haskell/Zippers
*)


module Zipper : sig
    type 'a node =
      | DeadEnd of 'a 
      | Passage of 'a * 'a node
      | Fork of 'a * 'a node * 'a node

    val get : 'a node -> 'a (* Get the value at a node *)

    val put : 'a -> 'a node -> 'a node (* Change the value at a node *)

    end = struct
    type 'a node =
    | DeadEnd of 'a 
    | Passage of 'a * 'a node
    | Fork of 'a * 'a node * 'a node

    let get  (n : 'a node) : 'a =
        match n with
        | DeadEnd x -> x
        | Passage (x, _) -> x
        | Fork (x, _, _) -> x

    let put (v : 'a ) (n : 'a node ) : 'a node =
        match n with
        | DeadEnd _ -> DeadEnd v
        | Passage (_, a) -> Passage (v, a)
        | Fork (_, a, b) -> Fork (v, a, b)

    end
