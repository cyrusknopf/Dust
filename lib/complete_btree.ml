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

        let get_lesser (a : 'a node) (b : 'a node) : 'a node =
            if a.h > b.h then a else b

        (*TODO TEST*)
        let rec backprop (n : 'a node) : 'a tree = 
            match n.p with
            | None -> { root = Some n }
            | Some parent -> backprop { parent with h = parent.h + 1 }

        let rec push (t : 'a tree) (n : 'a node) : 'a tree =
            match t.root with
            | None -> { root = Some n }
            | Some r ->
                match r.l, r.r with
                | Some left, Some right ->
                    push { root = Some (get_lesser left right) } n
                | Some _, None ->
                        { root = Some { r with r = Some { n with p = Some r } } }
                | None, Some _ ->
                        { root = Some { r with l = Some { n with p = Some r } } }
                | None, None ->
                        { root = Some { r with l = Some { n with p = Some r } } }
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
                | None -> [%expect.unreachable]
    )
    | None -> [%expect.unreachable]

let %expect_test "test-push-two" =
    let parent = { CompleteBTree.v = 'a'; h = 0; p = None; l = None; r = None } in
    let child1 = { CompleteBTree.v = 'b'; h = 0; p = None; l = None; r = None } in
    let child2 = { CompleteBTree.v = 'c'; h = 0; p = None; l = None; r = None } in
    let tree = { CompleteBTree.root = Some parent } in
    let tree' = CompleteBTree.push tree child1 in
    let tree'' = CompleteBTree.push tree' child2 in
    match tree''.root with
    | Some r -> (
            print_char r.v;
            [%expect {| a |}];
            (match r.l with
                | Some c ->
                        print_char c.v;
                        [%expect {| b |}];
                | None -> [%expect.unreachable]);
            match r.r with 
            | Some c ->
                    print_char c.v;
                    [%expect {| c |}];
            | None -> [%expect.unreachable]

    )
    | None -> [%expect.unreachable]
