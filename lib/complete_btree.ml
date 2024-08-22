module CompleteBTree =
    struct
        type 'a node = {
            v : 'a;
            h : int;
            p : 'a node option;
            l : 'a node option;
            r : 'a node option;
        }

        type 'a tree = {
            root : 'a node option;
        }

        exception BadStructure of string

        let make_node (value : 'a) : 'a node = 
            { v = value; h = 0; p = None; l = None; r = None }

        let get_lesser (a : 'a node) (b : 'a node) : 'a node =
            if a.h > b.h then a else b

        let rec backprop (n : 'a node) (h : int) : 'a tree = 
            match n.p with
            | None -> { root = Some n }
            | Some parent -> backprop { parent with h = h } (h + 1)

        let rec push (t : 'a tree) (n : 'a node) : 'a tree =
            match t.root with
            | None -> { root = Some n }
            | Some r ->
                match r.l, r.r with
                | Some left, Some right ->
                        push { root = Some (get_lesser left right) } n (*TODO This case doesn't update correctly*)
                | Some _, None ->
                        backprop { n with p = Some {r with r = Some n } } 1
                | None, Some _ -> (*Should never match *)
                        raise ( BadStructure "Should never be a right child with no left" )
                | None, None ->
                        backprop { n with p = Some { r with l = Some n } } 1
    end

(* TESTS *)
let %expect_test "test-push-single" =
    let parent = { CompleteBTree.v = 'a'; h = 0; p = None; l = None; r = None } in
    let child = { CompleteBTree.v = 'b'; h = 0; p = None; l = None; r = None } in
    let tree = { CompleteBTree.root = Some parent } in
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
                        print_int r.h;
                        [%expect {| 1 |}];
                | None -> [%expect.unreachable]
    )
    | None -> [%expect.unreachable]

let %expect_test "test-push-two" =
    let parent = CompleteBTree.make_node 'a' in
    let child1 = CompleteBTree.make_node 'b' in
    let child2 = CompleteBTree.make_node 'c' in
    let tree = { CompleteBTree.root = Some parent } in
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
                        print_int r.h;
                        [%expect {| 1 |}];
                        print_int c1.h;
                        [%expect {| 0 |}];
                        print_int c2.h;
                        [%expect {| 0 |}];
                | _ -> [%expect.unreachable]);
    | _ -> [%expect.unreachable]

let %expect_test "test-push-three" =
    let parent = CompleteBTree.make_node 'a' in
    let child1 = CompleteBTree.make_node 'b' in
    let child2 = CompleteBTree.make_node 'c' in
    let child3 = CompleteBTree.make_node 'd' in
    let tree = { CompleteBTree.root = Some parent } in
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
                        print_int r.h;
                        [%expect {| 2 |}];
                        print_int c1.h;
                        [%expect {| 1 |}];
                        print_int c2.h;
                        [%expect {| 0 |}];
                        match c1.l, c1.r with
                        | Some l', None ->
                            print_char l'.v;
                            [%expect {| d |}];
                        | _ -> [%expect.unreachable]
                )
                | _ -> [%expect.unreachable]
    )
    | _ -> [%expect.unreachable]

