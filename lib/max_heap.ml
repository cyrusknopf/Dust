module MaxHeap =
    struct
        type 'a node = {
            data : 'a;
            prio : int;
            l_child : 'a node option;
            r_child : 'a node option;
        }

        type 'a pqueue = {
            root : 'a node option;
        }

        let create_node v p = { data = v; prio = p; l_child = None; r_child = None }

        let init () : 'a pqueue = {root = None}


        let rec push (q : 'a pqueue) (n : 'a node) : 'a pqueue =
            match q.root with
            | None -> { root = Some n }
            | Some r ->
                if r.prio > n.prio then
                    { root = Some { r with l_child = (push { root = r.l_child } n).root } }
                else
                    { root = Some { r with r_child = (push { root = r.r_child } n).root } }
                        
        let getVal (n : 'a node) : 'a = n.data
    end
