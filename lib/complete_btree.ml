open Queue

module CompleteBTree =
    struct

        type 'a btree =
            | Empty
            | Node of {
                v : 'a;
                l : 'a btree;
                r : 'a btree;
            }

        exception BadStructure of string


        (* TODO implement BFS
        let push (t : 'a btree) (v : 'a) : 'a btree =
            let q = Queue.create () in
            let rec ins q =
                match Queue.take_opt q with
                | Some Empty -> Node { v = v; l = Empty; r = Empty}
                | Some (Node {l;r;_} as node) ->

                | _ -> t
            in ins q 
            *)



    end

        (*

        exception BadStructure of string

        let make_node (value : 'a) : 'a node = 
            { v = value ;  l = None; r = None }

        let make_tree (root : 'a node) : 'a tree = 
            { root = Some root }

        let rec get_height (node : 'a node option) : int =
            match node with
            | None -> 0
            | Some n -> 1 + max (get_height n.l) (get_height n.r)

        let rec push (t : 'a tree) (n : 'a node) : 'a tree =
            match t.root with
            | None -> { root = Some n }
            | Some r ->
                match r.l, r.r with
                | Some left, Some right ->
                        if get_height (Some left) < get_height (Some right) then
                            let new_right_subtree = push { root = Some right } n in
                            let new_root = Some { r with r = new_right_subtree.root } in
                            { root = new_root }
                        else
                            let new_left_subtree = push { root = Some left } n in
                            let new_root = Some { r with l = new_left_subtree.root } in
                            { root = new_root }
                | Some _, None -> { root = Some { r with r = Some n } }
                | None, Some _ -> (* Should never match *)
                        raise ( BadStructure "Should never be a right child with no left" )
                | None, None -> { root = Some { r with l = Some n } }
    end

(* TESTS *)
let %expect_test  "test-get_height-root" =
    let parent = CompleteBTree.make_node 'a' in
    let h = CompleteBTree.get_height (Some parent) in
    print_int h;
    [%expect {| 1 |}]

let %expect_test "test-get_height-only_child" =
    let parent = CompleteBTree.make_node 'a' in
    let t = CompleteBTree.make_tree parent in
    let child = CompleteBTree.make_node 'b' in
    let t' = CompleteBTree.push t child in
    let h = CompleteBTree.get_height t'.root in
    print_int h;
    [%expect {| 2 |}]

let %expect_test "test-get_height-two_children" =
    let child1 = CompleteBTree.make_node 'b' in
    let child2 = CompleteBTree.make_node 'c' in
    let parent  = {CompleteBTree.v = 'a'; l = Some child1; r = Some child2 } in
    let h = CompleteBTree.get_height (Some parent) in
    print_int h;
    [%expect {| 2 |}]

let %expect_test "test-get_height-two_generations" =
    let gchild = CompleteBTree.make_node 'c' in
    let child = {CompleteBTree.v = 'b'; l = Some gchild; r = None } in
    let parent = {CompleteBTree.v = 'a'; l = Some child; r = None } in
    let h = CompleteBTree.get_height (Some parent) in
    print_int h;
    [%expect {| 3 |}]


let %expect_test "test-get_height-two_generations_sibling" =
    let gchild = CompleteBTree.make_node 'c' in
    let child1 = {CompleteBTree.v = 'b'; l = Some gchild; r = None } in
    let child2 = {CompleteBTree.v = 'b'; l = None; r = None } in
    let parent = {CompleteBTree.v = 'a'; l = Some child1; r = Some child2 } in
    let h = CompleteBTree.get_height (Some parent) in
    print_int h;
    [%expect {| 3 |}]

let %expect_test "test-push-single" =
    let parent = CompleteBTree.make_node 'a' in
    let child = CompleteBTree.make_node 'b' in
    let tree = CompleteBTree.make_tree parent in
    let tree' = CompleteBTree.push tree child in
    match tree'.root with
    | Some r -> (
            print_char r.v;
            [%expect {| a |}];
            match r.l with
                | Some c ->
                        print_char c.v;
                        [%expect {| b |}];
                        print_string (string_of_bool (Option.is_none r.r ));
                        [%expect {| true |}];
                        print_int (CompleteBTree.get_height (Some r));
                        [%expect {| 2 |}];
                | None -> [%expect.unreachable]
    )
    | None -> [%expect.unreachable]

let %expect_test "test-push-two" =
    let parent = CompleteBTree.make_node 'a' in
    let child1 = CompleteBTree.make_node 'b' in
    let child2 = CompleteBTree.make_node 'c' in
    let tree = CompleteBTree.make_tree parent in
    let tree' = CompleteBTree.push tree child1 in
    let tree'' = CompleteBTree.push tree' child2 in
    match tree''.root with
    | Some r -> (
            print_char r.v;
            [%expect {| a |}];
            match r.l, r.r with
                | Some c1, Some c2 ->
                        print_char c1.v;
                        [%expect {| b |}];
                        print_char c2.v;
                        [%expect {| c |}];
                        print_int (CompleteBTree.get_height (Some r));
                        [%expect {| 2 |}];
                        print_int (CompleteBTree.get_height (Some c1));
                        [%expect {| 1 |}];
                        print_int (CompleteBTree.get_height (Some c2));
                        [%expect {| 1 |}];
                | _ -> [%expect.unreachable]);
    | _ -> [%expect.unreachable]

let %expect_test "test-push-three" =
    let parent = CompleteBTree.make_node 'a' in
    let child1 = CompleteBTree.make_node 'b' in
    let child2 = CompleteBTree.make_node 'c' in
    let child3 = CompleteBTree.make_node 'd' in
    let tree = CompleteBTree.make_tree parent  in
    let tree' = CompleteBTree.push tree child1 in
    let tree'' = CompleteBTree.push tree' child2 in
    let tree''' = CompleteBTree.push tree'' child3 in
    match tree'''.root with
    | Some r ->(
            print_char r.v;
            [%expect {| a |}];
            match r.l, r.r with
                | Some c1, Some c2 ->(
                        print_char c1.v;
                        [%expect {| b |}];
                        print_char c2.v;
                        [%expect {| c |}];
                        print_int (CompleteBTree.get_height (Some r));
                        [%expect {| 3 |}];
                        print_int (CompleteBTree.get_height (Some c1));
                        [%expect {| 2 |}];
                        print_int (CompleteBTree.get_height (Some c2));
                        [%expect {| 1 |}];
                        match c1.l, c1.r with
                        | Some l', None ->
                            print_char l'.v;
                            [%expect {| d |}];
                        | _ -> [%expect.unreachable]
                )
                | _ -> [%expect.unreachable]
    )
    | _ -> [%expect.unreachable]
*)
