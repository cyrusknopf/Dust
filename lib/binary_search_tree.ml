module BinarySearchTree =
    struct
        (* Binary search tree implementation *)
        (* Values are inserted to the right if they are equal *)
        type 'a bstree =
            | Empty                 (* Empty node *)

            | Node of {             (* Node with one or two children *)
                v : 'a;             (* Value of node *)
                l : 'a bstree;      (* Left child of node *)
                r : 'a bstree;      (* Right child of node *)
            }

        exception NotMem of string

        let rec maximum (t : 'a bstree) : 'a bstree =
            match t with
            | Node { r = right; _} when right <> Empty -> maximum right
            | _ -> t

        let rec minimum (t : 'a bstree) : 'a bstree =
            match t with
            | Node { l = left; _} when left <> Empty -> minimum left
            | _ -> t

        let rec successor (t : 'a bstree) : 'a bstree = t




        (* Insert a node into the tree *)
        let rec add (t : 'a bstree) (v : 'a) : 'a bstree =
            match t with
            | Empty -> Node{ v; l = Empty; r = Empty }
            | Node n when n.l = Empty && n.r = Empty ->
                    let child = Node { v; l = Empty; r = Empty } in
                    if n.v > v then
                        Node { v = n.v; l = child; r = Empty }
                    else
                        Node { v = n.v; l = Empty; r = child}

            (* Create new subtree with it added to the left *)
            | Node n when n.v > v -> Node { n with l = add n.l v }

            (* Create new subtree with it added to the right *)
            | Node n -> Node { n with r = add n.r v }
(*
        let rec remove (t : 'a bstree) (v : 'a) : 'a bstree =
            match t with
            | Leaf l when l.v = v -> Empty
            | Node n when n.v = v -> t

            | Node n when n.v > v -> Node { n with l = remove n.l v }
            | Node n -> Node { n with l = remove n.r v }
             | _ -> raise (NotMem "Value to remove was not found in tree") *)



        (* Determine whether a node is in the tree *)
        let rec find (t : 'a bstree) (v : 'a) : bool =
            match t with
            | Empty -> false
            (* Leaf whose value is not the one we are looking for -> false *)
            | Node { v = value; l  = Empty; r = Empty } when value <> v -> false
            (* Otherwise, if leaf then must have correct value -> true *)
            | Node { v = _; l  = Empty; r = Empty} -> true
            | Node { v = value; _} when value = v -> true (* If node val = v *)
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
    | Node l -> print_int l.v; [%expect {| 10 |}]
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
            | Node l, Empty -> print_int l.v; [%expect {| 1 |}]
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
            | Empty, Node l -> print_int l.v; [%expect {| 11 |}]
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
