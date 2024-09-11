(* 
    OCaml implemenation of a zipper inspired by this chapter:
    https://en.wikibooks.org/wiki/Haskell/Zippers
*)
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

    type branch =
    | KeepStraight
    | TurnLeft
    | TurnRight

    let turn_right (n : 'a node) : 'a node option =
        match n with
        | DeadEnd _ -> None
        | Passage _ -> None
        | Fork (_, _, b) -> Some b

    let turn_left (n : 'a node) : 'a node option =
        match n with
        | DeadEnd _ -> None
        | Passage _ -> None
        | Fork (_, a, _) -> Some a

    let keep_straight (n : 'a node) : 'a node option =
        match n with
        | DeadEnd _ -> None
        | Passage (_, a) -> Some a
        | Fork _ -> None

    type thread = branch list

    let turn_right (t : thread) : thread = t @ [TurnRight]

    let turn_left (t : thread) : thread = t @ [TurnLeft]

    let keep_straight (t : thread) : thread = t @ [KeepStraight]

    (* Given a thread and a node, retrieve the value at the end of the thread *)
    let rec retrieve (t : thread) (n : 'a node) : 'a =
        match t, n with
        | [], m -> get m
        | KeepStraight::bs, Passage (_, m) -> retrieve bs m
        | TurnLeft::bs, Fork (_, a, _) -> retrieve bs a
        | TurnRight::bs, Fork (_, _, b) -> retrieve bs b
        | _ -> failwith "Bad thread"

    (* Apply f to the value at n *)
    let update (f : 'a -> 'a) (n : 'a node) =
        match n with
        | DeadEnd x -> DeadEnd (f x)
        | Passage (x, a) -> Passage (f x, a)
        | Fork (x, a, b) -> Fork (f x, a, b)
