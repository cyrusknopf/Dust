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
            | Some node, None -> Some node (* Some node none *)
            | None, Some node -> Some node (*               -> the other node *)
            | Some node1, Some node2 -> (* Both some -> greater of the two *)
                    if node1.value > node2.value then Some node1 else Some node2

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
            | Some r ->
                    match r.l_child, r.r_child with
                    | None, None -> { root = Some r } (* No children -> return itself *)
                    | Some left, None -> (* Only left child -> swap if larger *)
                            if left.value > r.value then
                                let new_root = { left with l_child = Some r } in
                                { root = Some new_root }
                            else { root = Some r }
                    | None, Some right -> (* Only right child -> swap if larger *)
                            if right.value > r.value then
                                let new_root = { right with r_child = Some r} in
                                { root = Some new_root }
                            else { root = Some r }
                    | Some left, Some right -> q

            

        let rec push (q : 'a pqueue) (n : 'a node) : 'a pqueue =
            match q.root with
            | None -> { root = Some n }
            | Some r -> q
    end
