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
                                heapify { root = Some new_root }
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

(* TESTS *)
let%expect_test "test-init-()" =
    let mh = MaxHeap.init () in
    print_string (string_of_bool (Option.is_none mh.root));
    [%expect {| true |}]

let%expect_test "test-get_max-left_greater" =
    let a = MaxHeap.create_node 'a' 10 in
    let b = MaxHeap.create_node 'b' 1 in
    let res = (MaxHeap.get_max (Some a) (Some b)) in
    match res with
    | Some r -> print_char r.value;
                [%expect {| a |}]
    | None -> [%expect.unreachable]

let%expect_test "test-get_max-right" =
    let x = MaxHeap.create_node 'x' 10 in
    let y = MaxHeap.create_node 'y' 1 in
    let res = (MaxHeap.get_max (Some y) (Some x)) in
    match res with
    | Some r -> print_char r.value;
                [%expect {| x |}]
    | None -> [%expect.unreachable]

let%expect_test "test-heapify-root" = 
    let r = MaxHeap.create_node "root" 10 in
    let mh = { MaxHeap.root = Some r } in
    let mh' = MaxHeap.heapify mh in
    match mh'.root with
    | Some r -> print_string r.value;
                [%expect {| root |}]
    | None -> [%expect.unreachable]

let%expect_test "test-heapify-lchild" =
    let lc = MaxHeap.create_node "lchild" 15 in
    let r = { MaxHeap.value = "root"; prio = 10; l_child = Some lc; r_child = None } in
    let mh = { MaxHeap.root = Some r } in
    let mh' = MaxHeap.heapify mh in
    match mh'.root with
    | Some r -> print_string r.value;
                [%expect {| lchild |}]
    | _ -> [%expect.unreachable]

let%expect_test "test-heapify-rchild" =
    let rc = MaxHeap.create_node "rchild" 15 in
    let r = { MaxHeap.value = "root"; prio = 10; r_child = Some rc; l_child = None } in
    let mh = { MaxHeap.root = Some r } in
    let mh' = MaxHeap.heapify mh in
    match mh'.root with
    | Some r -> print_string r.value;
                [%expect {| rchild |}]
    | _ -> [%expect.unreachable]

let%expect_test "test-heapify-rchild_greater" =
    let rc = MaxHeap.create_node "rchild" 20 in
    let lc = MaxHeap.create_node "lchild" 15 in
    let r = { MaxHeap.value = "root"; prio = 10; r_child = Some rc; l_child = Some lc } in
    let mh = { MaxHeap.root = Some r } in
    let mh' = MaxHeap.heapify mh in
    match mh'.root with
    | Some r -> print_string r.value;
                [%expect {| rchild |}]
    | _ -> [%expect.unreachable]

let%expect_test "test-heapify-lchild_greater" =
    let rc = MaxHeap.create_node "rchild" 20 in
    let lc = MaxHeap.create_node "lchild" 30 in
    let r = { MaxHeap.value = "root"; prio = 10; r_child = Some rc; l_child = Some lc } in
    let mh = { MaxHeap.root = Some r } in
    let mh' = MaxHeap.heapify mh in
    match mh'.root with
    | Some r -> print_string r.value;
                [%expect {| lchild |}]
    | _ -> [%expect.unreachable]

(* XXX This swaps the child, and then grandchild *)
let%expect_test "test-heapify-rgchild_greatest" =
    let rgc = MaxHeap.create_node "rgchild" 20 in
    let lgc = MaxHeap.create_node "lgchild" 15 in
    let lc = { MaxHeap.value = "lchild"; prio = 12; r_child = Some rgc; l_child = Some lgc } in
    let r = { MaxHeap.value = "root"; prio = 10; r_child = None; l_child = Some lc } in
    let mh = { MaxHeap.root = Some r } in
    let mh' = MaxHeap.heapify mh in
    match mh'.root with
    | Some r -> print_string r.value;
                [%expect {| rgchild |}]
    | _ -> [%expect.unreachable]
