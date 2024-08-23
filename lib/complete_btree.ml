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


        let push (t : 'a btree) (v : 'a) : 'a btree =
            let rec ins (q: 'a btree t) : 'a btree =
            let parent = Queue.take_opt q in
                match parent with
                | None -> raise (BadStructure "Wrongly defined tree")
                | Some Empty -> Node { v = v; l = Empty; r = Empty}
                | Some Node node ->
                        let l = node.l in
                        let r = node.r in
                        match l,r with
                        | Empty, _ -> Node { node with l = (Node { v = v; l = Empty; r = Empty }) }
                        | _, Empty -> Node { node with r = (Node { v = v; l = Empty; r = Empty }) }
                        | Node _, Node _ ->
                                let () = Queue.add l q in
                                let () = Queue.add r q in
                                Node { node with l = ins q }
            in
            let q = Queue.create () in
            Queue.add t q;
            ins q

    end

(* TESTS *)
let %expect_test "test-push-one" = 
    let tree = CompleteBTree.Empty in
    let tree' = CompleteBTree.push tree 1 in
    match tree' with
    | Node n ->
            print_int n.v;
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
            | Node l ->
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
            | Node l, Node r ->
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
            | Node l, Node r ->(
                print_int l.v;
                [%expect {| 2 |}];
                print_int r.v;
                [%expect {| 3 |}];
                match l.l, l.r with
                    | Node g, Empty ->
                        print_int g.v;
                        [%expect {| 4 |}];
                    | _ -> [%expect.unreachable])
            | _ -> [%expect.unreachable])
    | _ -> [%expect.unreachable]

let %expect_test "test-push-five" = 
    let tree = CompleteBTree.Empty in
    let tree' = CompleteBTree.push tree 1 in
    let tree'' = CompleteBTree.push tree' 2 in
    let tree''' = CompleteBTree.push tree'' 3 in
    let tree'''' = CompleteBTree.push tree''' 4 in
    let tree''''' = CompleteBTree.push tree'''' 5 in
    match tree''''' with
    | Node n ->(
            print_int n.v;
            [%expect {| 1 |}];
            match n.l, n.r with
            | Node l, Node r ->(
                print_int l.v;
                [%expect {| 2 |}];
                print_int r.v;
                [%expect {| 3 |}];
                match l.l, l.r with
                    | Node g, Node h ->
                        print_int g.v;
                        [%expect {| 4 |}];
                        print_int h.v;
                        [%expect {| 5 |}];
                    | _ -> [%expect.unreachable])
            | _ -> [%expect.unreachable])
    | _ -> [%expect.unreachable]

let %expect_test "test-push-six" = 
    let tree = CompleteBTree.Empty in
    let tree = CompleteBTree.push tree 1 in
    let tree = CompleteBTree.push tree 2 in
    let tree = CompleteBTree.push tree 3 in
    let tree = CompleteBTree.push tree 4 in
    let tree = CompleteBTree.push tree 5 in
    let tree = CompleteBTree.push tree 6 in
    CompleteBTree.print_btree "" false tree;
    match tree with
    | Node n ->(
            print_int n.v;
            [%expect {| 1 |}];
            match n.l, n.r with
            | Node l, Node r ->(
                print_int l.v;
                [%expect {| 2 |}];
                print_int r.v;
                [%expect {| 3 |}];
                (match l.l, l.r with
                    | Node g, Node h ->
                        print_int g.v;
                        [%expect {| 4 |}];
                        print_int h.v;
                        [%expect {| 5 |}];
                    | _ -> [%expect.unreachable]);
                (match r.l, r.r with
                    | Node j, Empty ->
                            print_int j.v;
                            [%expect {| 6 |}];
                    | _ -> [%expect.unreachable]);)
            | _ -> [%expect.unreachable])
    | _ -> [%expect.unreachable]
