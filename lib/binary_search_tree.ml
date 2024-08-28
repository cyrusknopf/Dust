module BinarySearchTree =
    struct
        (* Binary search tree implementation *)
        (* Values are inserted to the right if they are equal *)
        type 'a bstree =
            | Empty                 (* Empty node *)

            | Leaf of {             (* Node with no children*)
                v : 'a              (* Value of node *)
            }

            | Node of {             (* Node with one or two children *)
                v : 'a;             (* Value of node *)
                l : 'a bstree;      (* Left child of node *)
                r : 'a bstree;      (* Right child of node *)
            }

        (* Insert a node into the tree *)
        let rec add (t : 'a bstree) (v : 'a) : 'a bstree =
            match t with
            | Empty ->
                    Leaf { v }
            | Leaf l ->
                    let child = Leaf { v } in
                    if l.v > v then
                        Node { v = l.v; l = child; r = Empty }
                    else
                        Node { v = l.v; l = Empty; r = child}
            | Node n when n.v > v ->
                    Node { n with l = add n.l v } (* Create new subtree with it added to the left *)
            | Node n ->
                    Node { n with r = add n.r v } (* Create new subtree with it added to the right *)

        (* Determine whether a node is in the tree *)
        let rec find (t : 'a bstree) (v : 'a) : bool =
            match t with
            | Empty -> false
            | Leaf l when l.v <> v -> false
            | Leaf _ -> true (* If above case does not match, then leaf v = v -> found *)
            | Node n when n.v = v -> true (* If node val = v *)
            | Node n when n.v > v -> (* If node value greater, search left *)
                    find n.l v
            | Node n -> (* If node value lesser or equal, search right *)
                    find n.r v
    end

(* TESTS *)
let %expect_test "test-add-empty" = 
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    match tree with
    | Leaf l -> print_int l.v; [%expect {| 10 |}]
    | _ -> [%expect.unreachable]

let %expect_test "test-add-child_lesser" =
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = add tree 1 in
    match tree with
    | Node n -> (
            print_int n.v; [%expect {| 10 |}];
            match n.l, n.r with
            | Leaf l, Empty -> print_int l.v; [%expect {| 1 |}]
            | _ -> [%expect.unreachable])
    | _ -> [%expect.unreachable]

let %expect_test "test-add-child_greater" =
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = add tree 11 in
    match tree with
    | Node n -> (
            print_int n.v; [%expect {| 10 |}];
            match n.l, n.r with
            | Empty, Leaf l -> print_int l.v; [%expect {| 11 |}]
            | _ -> [%expect.unreachable])
    | _ -> [%expect.unreachable]


let %expect_test "test-find-root" = 
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let mem = find tree 10 in
    print_string (string_of_bool mem);
    [%expect {| true |}];
    let mem = find tree 1 in
    print_string (string_of_bool mem);
    [%expect {| false |}]

let %expect_test "test-find-root" = 
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = add tree 1 in
    let tree = add tree 11 in

    let mem = find tree 10 in
    print_string (string_of_bool mem);
    [%expect {| true |}];

    let mem = find tree 10 in
    print_string (string_of_bool mem);
    [%expect {| true |}];

    let mem = find tree 11 in
    print_string (string_of_bool mem);
    [%expect {| true |}];

    let mem = find tree 1 in
    print_string (string_of_bool mem);
    [%expect {| true |}];

    let mem = find tree 0 in
    print_string (string_of_bool mem);
    [%expect {| false |}]
