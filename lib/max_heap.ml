module type Ordered =
    sig
        type t
        val leq : t -> t -> bool
    end

module MaxHeap =
    struct
        type 'a node = {
            value : 'a;
            prio : int;
            l_child : 'a node option;
            r_child : 'a node option;
        }

        type 'a pqueue = {
            root : 'a node option;
        }

        let get_max (p :'a node option) (q:  'a node option) : 'a node option =
            match p,q with
            | None, None -> None (* Both none -> None *)
            | Some node, None -> Some node (* One node none                   *)
            | None, Some node -> Some node (*               -> the other node *)
            | Some node1, Some node2 -> (* Both some -> greater of the two *)
                    if node1.prio > node2.prio then Some node1 else Some node2

        let create_node (v : 'a) (p : int) : 'a node =
            {
                value = v;
                prio = p;
                l_child = None;
                r_child = None
            }

        let init () : 'a pqueue = { root = None }

        let rec heapify (q : 'a pqueue) : 'a pqueue =
            match q.root with
            | None -> q
            | Some root ->
                    match root.l_child, root.r_child with
                    | None, None -> { root = Some root } (* No children -> return itself *)

                    | Some left, None -> (* Only left child -> swap if larger *)
                            if left.prio > root.prio then
                                let new_root = swapl root left in
                                { root = Some new_root }
                            else { root = Some root }

                    | None, Some right -> (* Only right child -> swap if larger *)
                            if right.prio > root.prio then
                                let new_root = swapr root right in
                                { root = Some new_root }
                            else { root = Some root }

                    | Some left, Some right ->
                            match left, right with
                            | l, r when l.prio > root.prio && l.prio > r.prio ->
                                    let new_root = swapl root left in
                                    { root = Some new_root }
                            | l, r when r.prio > root.prio && r.prio > l.prio ->
                                let new_root = swapr root right in
                                { root = Some new_root}
                            | _ -> { root = Some root}

            and swapl (parent : 'a node ) (child : 'a node ) : 'a node =
            let new_child = { parent with l_child = child.l_child; r_child = child.r_child } in
            let new_subtree = heapify { root = Some new_child } in
            { child with l_child = new_subtree.root }

            and swapr (parent : 'a node ) (child : 'a node ) : 'a node =
            let new_child = { parent with l_child = child.l_child; r_child = child.r_child } in
            let new_subtree = heapify { root = Some new_child } in
            { child with r_child = new_subtree.root }



        
        let rec push (q : 'a pqueue) (n : 'a node) : 'a pqueue =
            match q.root with
            | None -> { root = Some n }
            | Some r ->  push q r
    end
