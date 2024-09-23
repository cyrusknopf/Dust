(*
    First zipper implementation using the motivation of a labyrinth 
    with paths forward, forks with left and right paths, and dead ends;
    found in the following chapter:
    https://en.wikibooks.org/wiki/Haskell/Zippers
 *)

module AriadnesZipper : sig
        (* Node representing a point in the labyrinth *)
        type 'a node =
        | DeadEnd of 'a 
        | Passage of 'a * 'a node
        | Fork of 'a * 'a node * 'a node

        (* Stores the data of the node the branch is taken from,
           as well as the subtree lost by the untaken branch *)
        type 'a branch =
        | KeepStraight of 'a 
        | TurnLeft of 'a * 'a node
        | TurnRight of 'a * 'a node

        (* List of branches for zipper *)
        type 'a thread = 'a branch list

        (* Thread + current node = zipper *)
        type 'a zipper = 'a thread * 'a node 

        val get : 'a zipper -> 'a

        val set : 'a -> 'a zipper -> 'a zipper

        val apply : ('a -> 'a) -> 'a zipper -> 'a zipper

        val keep_straight : 'a zipper -> 'a zipper option

        val turn_left : 'a zipper -> 'a zipper option

        val turn_right : 'a zipper -> 'a zipper option

        val back : 'a zipper -> 'a zipper option

    end = struct
        (* Nodes in the tree, with no, one or two children *)
        type 'a node =
        | DeadEnd of 'a 
        | Passage of 'a * 'a node
        | Fork of 'a * 'a node * 'a node

        (* Stores the data of the node the branch is taken from,
           as well as the subtree lost by the untaken branch *)
        type 'a branch =
        | KeepStraight of 'a 
        | TurnLeft of 'a * 'a node
        | TurnRight of 'a * 'a node

        (* List of branches which describe the path taken to reach an anonymous node *)
        type 'a thread = 'a branch list

        (* Current node and thread to reach the current node *)
        type 'a zipper = 'a thread * 'a node 

        (* Gets the value of the current node represented by the zipper *)
        let get (z : 'a zipper) : 'a = 
            match z with
            | (_, DeadEnd x) -> x
            | (_, Passage (x, _)) -> x
            | (_, Fork (x, _, _)) -> x

        (* Sets the value of the current node represented by the zipper *)
        let set (v : 'a ) (z : 'a zipper) : 'a zipper =
            match z with
            | (ts, DeadEnd _) -> (ts, DeadEnd v)
            | (ts, Passage (_, s)) -> (ts, Passage (v, s))
            | (ts, Fork (_, l, r)) -> (ts, Fork (v, l, r))
            
        (* Applies a function to the value of the current node represented by the zipper *)
        let apply (f : 'a -> 'a) (z : 'a zipper) : 'a zipper =
            match z with
            | (ts, DeadEnd x) -> (ts, DeadEnd (f x))
            | (ts, Passage (x, s)) -> (ts, Passage (f x, s))
            | (ts, Fork (x, l, r)) -> (ts, Fork (f x, l, r))

        (* Moves the node of the zipper, whilst saving the value of the node
           lost by making move *)
        let keep_straight (z : 'a zipper) : 'a zipper option =
            match z with
            | (ts, Passage (x, s)) ->
                    let new_branch = KeepStraight x in
                    Some (new_branch :: ts, s)
            | _ -> None

        (* Moves the node of the zipper, whilst saving the value of the node
           which the move is made from, along with the subtree lost by making move *)
        let turn_left (z : 'a zipper) : 'a zipper option =
            match z with
            | (ts, Fork (x, l, r)) ->
                    let new_branch = TurnLeft (x, r) in
                    Some (new_branch :: ts, l)
            | _ -> None

        (* Moves the node of the zipper, whilst saving the value of the node
           which the move is made from, along with the subtree lost by making move *)
        let turn_right (z : 'a zipper) : 'a zipper option =
            match z with
            | (ts, Fork (x, l, r)) ->
                    let new_branch = TurnRight (x, l) in
                    Some (new_branch :: ts, r)
            | _ -> None

        (* Traverse one step back in the zipper, returning the resultant zipper *)
        let back (z : 'a zipper) : 'a zipper option =
            match z with
            | ([], _) -> None (* No more thread => we are at the root *)
            | (KeepStraight x :: ts, n) -> Some (ts, Passage (x, n)) (* Moved forward => construct passage *)
            | (TurnLeft (x, r)::ts, l) -> Some (ts, Fork (x, l, r )) (* Turned left => construct fork *)
            | (TurnRight (x, l)::ts, r) -> Some (ts, Fork (x, l, r)) (* Turned right => construct fork *)
    end
        include AriadnesZipper
