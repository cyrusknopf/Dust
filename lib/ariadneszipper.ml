module AriadnesZipper : sig
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

        val get : 'a node -> 'a

        val set : 'a -> 'a node -> 'a node

        val apply : ('a -> 'a) -> 'a node -> 'a node

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

        (* Get the value of a node *)
        let get  (n : 'a node) : 'a =
            match n with
            | DeadEnd x -> x
            | Passage (x, _) -> x
            | Fork (x, _, _) -> x

        (* Set the value of a node *)
        let set (v : 'a ) (n : 'a node ) : 'a node =
            match n with
            | DeadEnd _ -> DeadEnd v
            | Passage (_, s) -> Passage (v, s)
            | Fork (_, l, r) -> Fork (v, l, r)

        (* Apply a function, `f`, to the value of a node *)
        let apply (f : 'a -> 'a) (n : 'a node) : 'a node =
            match n with
            | DeadEnd x -> DeadEnd (f x)
            | Passage (x, s) -> Passage (f x, s)
            | Fork (x, l, r) -> Fork (f x, l, r)

        (* Stores the data of the node the branch is taken from,
           as well as the subtree lost by the untaken branch *)
        type 'a branch =
        | KeepStraight of 'a 
        | TurnLeft of 'a * 'a node
        | TurnRight of 'a * 'a node

        type 'a thread = 'a branch list

        type 'a zipper = 'a thread * 'a node 

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
