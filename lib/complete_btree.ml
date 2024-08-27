module CompleteBTree =
    struct

        type 'a btree =
            | Empty
            | Leaf of {
                v : 'a;
            }
            | Node of {
                v : 'a;
                l : 'a btree;
                r : 'a btree;
            }

        exception BadStructure of string

        let exp x y = (float_of_int x) ** (float_of_int y) |> int_of_float

        let rec height (node  : 'a btree) : int =
            match node with
            | Node n -> 1 + max (height n.l) (height n.r)
            | _ -> 0

        (* TODO make tail recursive *)
        let rec size (node : 'a btree) : int =
            match node with
            | Empty -> 0
            | Leaf _ -> 1
            | Node n -> 1 + size n.l + size n.r

        let max_size (t : 'a btree) : int =
            let h = height t in
            match h with
            | 0 -> 0
            | _ -> exp 2 (h+1) - 2

        let rec push (t : 'a btree) (v : 'a) : 'a btree =
            match t with
            | Leaf l -> Node { v = l.v; l = Leaf { v }; r = Empty}

            | Node n when n.l = Empty ->
                    Node { n with l = Leaf { v } }

            | Node n when n.r = Empty ->
                    Node { n with r = Leaf { v } }

            | Node n when max_size n.l - 1 > size n.l ->
                    Node { n with l = push n.l v }

            | Node n when max_size n.r - 1 > size n.r ->
                    Node { n with r = push n.r v }

            | Node n ->
                    Node { n with l = push n.l v }

            | Empty -> Leaf { v }


    end

(* TESTS *)
let %expect_test "test-height-root" =
    let tree = CompleteBTree.Leaf{v=0} in
    let h = CompleteBTree.height tree in
    print_int h;
    [%expect {| 0 |}]

let %expect_test "test-height-root_children" =
    let tree = CompleteBTree.Node{ v = 0; l = Leaf{v=1}; r = Leaf{v=1}} in
    let h = CompleteBTree.height tree in
    print_int h;
    [%expect {| 1 |}]

let %expect_test "test-max_size-root" =
    let tree = CompleteBTree.Leaf{v=0} in
    let m = CompleteBTree.max_size tree in
    print_int m;
    [%expect {| 0 |}]

let %expect_test "test-max_size-root_children" =
    let tree = CompleteBTree.Node{ v = 0; l = Leaf{v=1}; r = Leaf{v=1}} in
    let m = CompleteBTree.max_size tree in
    print_int m;
    [%expect {| 2 |}]

let %expect_test "test-max_size-three_generations_full" =
    let left = CompleteBTree.Node
        {
            v = 10;
            l = Leaf{ v = 100};
            r = Leaf{ v = 101}
        } in
    let right = CompleteBTree.Node
        {
            v = 11;
            l = Leaf{ v = 111};
            r = Leaf{ v = 112}
        } in
    let root = CompleteBTree.Node
        {
            v = 1;
            l = left;
            r = right
        } in
    let m = CompleteBTree.max_size root in
    print_int m;
    [%expect {| 6 |}]

let %expect_test "test-max_size-three_generations_half" =
    let left = CompleteBTree.Node
        {
            v = 10;
            l = Leaf{ v = 100};
            r = Leaf{ v = 101}
        } in
    let right = CompleteBTree.Leaf {v = 11} in
    let root = CompleteBTree.Node
        {
            v = 1;
            l = left;
            r = right
        } in
    let m = CompleteBTree.max_size root in
    print_int m;
    [%expect {| 6 |}]

let %expect_test "test-push-one" = 
    let tree = CompleteBTree.Empty in
    let tree' = CompleteBTree.push tree 1 in
    match tree' with
    | Leaf l ->
            print_int l.v;
            [%expect {| 1 |}]
    | _ -> [%expect.unreachable]

let %expect_test "test-push-two" = 
    let tree = CompleteBTree.Empty in
    let tree' = CompleteBTree.push tree 1 in
    let tree'' = CompleteBTree.push tree' 2 in
    match tree'' with
    | Node n ->(
            print_int n.v;
            [%expect {| 1 |}];
            match n.l with
            | Leaf l ->
                print_int l.v;
                [%expect {| 2 |}];
            | _ -> [%expect.unreachable])
    | _ -> [%expect.unreachable]

let %expect_test "test-push-three" = 
    let tree = CompleteBTree.Empty in
    let tree' = CompleteBTree.push tree 1 in
    let tree'' = CompleteBTree.push tree' 2 in
    let tree''' = CompleteBTree.push tree'' 3 in
    match tree''' with
    | Node n ->(
            print_int n.v;
            [%expect {| 1 |}];
            match n.l, n.r with
            | Leaf l, Leaf r ->
                print_int l.v;
                [%expect {| 2 |}];
                print_int r.v;
                [%expect {| 3 |}];
            | _ -> [%expect.unreachable])
    | _ -> [%expect.unreachable]

let %expect_test "test-push-four" = 
    let tree = CompleteBTree.Empty in
    let tree' = CompleteBTree.push tree 1 in
    let tree'' = CompleteBTree.push tree' 2 in
    let tree''' = CompleteBTree.push tree'' 3 in
    let tree'''' = CompleteBTree.push tree''' 4 in
    match tree'''' with
    | Node n ->(
            print_int n.v;
            [%expect {| 1 |}];
            match n.l, n.r with
            | Node l, Leaf r ->(
                print_int l.v;
                [%expect {| 2 |}];
                print_int r.v;
                [%expect {| 3 |}];
                match l.l, l.r with
                    | Leaf g, Empty ->
                        print_int g.v;
                        [%expect {| 4 |}];
                    | _ -> [%expect.unreachable])
            | _ -> [%expect.unreachable])
    | _ -> [%expect.unreachable]

let %expect_test "test-push-five" = 
    let tree = CompleteBTree.Empty in
    let tree = CompleteBTree.push tree 1 in
    let tree = CompleteBTree.push tree 2 in
    let tree = CompleteBTree.push tree 3 in
    let tree = CompleteBTree.push tree 4 in
    let tree = CompleteBTree.push tree 5 in
    match tree with
    | Node n ->(
            print_int n.v;
            [%expect {| 1 |}];
            match n.l, n.r with
            | Node l, Leaf r ->(
                print_int l.v;
                [%expect {| 2 |}];
                print_int (CompleteBTree.size n.l);
                [%expect {| 3 |}];
                print_int (CompleteBTree.height n.l);
                [%expect {| 1 |}];
                print_int (CompleteBTree.max_size n.l);
                [%expect {| 2 |}];

                print_int (CompleteBTree.size n.r);
                [%expect {| 1 |}];
                print_int (CompleteBTree.height n.r);
                [%expect {| 0 |}];
                print_int (CompleteBTree.max_size n.r);
                [%expect {| 0 |}];

                print_int r.v;
                [%expect {| 3 |}];
                match l.l, l.r with
                    | Leaf g, Leaf h ->
                        print_int g.v;
                        [%expect {| 4 |}];
                        print_int h.v;
                        [%expect {| 5 |}];
                    | _ -> [%expect.unreachable])
            | _ -> [%expect.unreachable])
    | _ -> [%expect.unreachable]

    (* Goes left because max_size right = 0 and size right = 1 *)
let %expect_test "test-push-six" = 
    let tree = CompleteBTree.Empty in
    let tree = CompleteBTree.push tree 1 in
    let tree = CompleteBTree.push tree 2 in
    let tree = CompleteBTree.push tree 3 in
    let tree = CompleteBTree.push tree 4 in
    let tree = CompleteBTree.push tree 5 in
    print_int (CompleteBTree.max_size tree);
    [%expect {| 6 |}];
    print_int (CompleteBTree.size tree);
    [%expect {| 5 |}];
    let tree = CompleteBTree.push tree 6 in
    print_int (CompleteBTree.max_size tree);
    [%expect {| 14 |}];
    print_int (CompleteBTree.size tree);
    [%expect {| 6 |}];
    match tree with
    | Node n ->(
            print_int n.v;
            [%expect {| 1 |}];
            print_int (CompleteBTree.max_size n.l);
            [%expect {| 2 |}];
            print_int (CompleteBTree.size n.l);
            [%expect {| 1 + 2 |}];
            match n.l, n.r with
            | Node l, Node r ->(
                print_int l.v;
                [%expect {| 2 |}];
                print_int r.v;
                [%expect {| 3 |}];)
            | _ ->[%expect.unreachable])
    | _ -> [%expect.unreachable]
    (*
                (match l.l, l.r with
                    | Leaf g, Leaf h ->
                        print_int g.v;
                        [%expect {| 4 |}];
                        print_int h.v;
                        [%expect {| 5 |}];
                    | _ -> [%expect.unreachable]);
                (match r.l, r.r with
                    | Leaf j, Empty ->
                            print_int j.v;
                            [%expect {| 6 |}];
                    | _ -> [%expect.unreachable]);)
            | _ -> [%expect.unreachable])
    | _ -> [%expect.unreachable]
    *)
