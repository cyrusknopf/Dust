module type PrioQueue =
    sig
        type 'a node = {
            value : 'a;
            prio : int;
            l_child : 'a node option;
            r_child : 'a node option;
        }
        type 'a pqueue = {
            root : 'a node option;
        }

        val init : unit -> 'a pqueue

        val enqueue : 'a pqueue -> 'a node -> 'a pqueue
        val getVal : 'a node -> 'a

        (*
        val dequeue : 'a pqueue -> 'a option
        *)
    end

module MaxHeap : PrioQueue =
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

        let init () : 'a pqueue = {root = None}

        let rec enqueue (q : 'a pqueue) (n : 'a node) : 'a pqueue =
            match q.root with
            | None -> { root = Some n }
            | Some r ->
                   match r.l_child with
                   | Some l -> if n.prio> l.prio then enqueue { root=r.l_child } n else enqueue { root=r.r_child } n
                   | None -> {root=None}

        let getVal (n : 'a node) : 'a = n.value
    end
