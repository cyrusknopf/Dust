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
        exception BadTraversal of string

        let rec height (t : 'a bstree) : int =
            match t with
            | Node n -> 1 + max (height n.l) (height n.r)
            | Empty -> 0

        (* Linearly prints a tree with a node's left child in {} and right in [] *)
        (* Leaves are followed by a . *)
        let rec print_bstree tree : unit =
            match tree with
            | Empty -> print_string ""
            | Node {v=v; l=Empty; r=Empty} -> print_int v; print_string "."
            | Node {v=v; l=l;r=Empty} -> print_int v; 
            print_string "{"; print_bstree l; print_string "}"
            | Node {v=v; l=Empty;r=r} -> print_int v; 
            print_string "["; print_bstree r; print_string "]"
            | Node n -> print_int n.v;
            print_string "{"; print_bstree n.l; print_string "}";
            print_string "["; print_bstree n.r; print_string "]"

        let rec maximum (t : 'a bstree) : 'a bstree =
            match t with
            | Node { r = right; _} when right <> Empty -> maximum right
            | _ -> t

        let rec minimum (t : 'a bstree) : 'a bstree =
            match t with
            | Node { l = left; _} when left <> Empty -> minimum left
            | _ -> t

        (* Insert a node into the tree *)
        let rec add (t : 'a bstree) (v : 'a) : 'a bstree =
            match t with
            | Empty -> Node { v; l = Empty; r = Empty }
            | Node { v = value; l = Empty; r = Empty } ->
                    let child = Node { v; l = Empty; r = Empty } in
                    if value > v then
                        Node { v = value; l = child; r = Empty }
                    else if value <= v then
                        Node { v = value; l = Empty; r = child}
                    else
                        raise (BadTraversal "Failed to traverse when adding")

            (* Create new subtree with it added to the left *)
            | Node n when n.v > v -> Node { n with l = add n.l v }

            (* Create new subtree with it added to the right *)
            | Node n when n.v <= v -> Node { n with r = add n.r v }
            | _ -> raise (BadTraversal "Failed to traverse node when adding")

        let rec replace_successor (t : 'a bstree) (v : 'a) : 'a bstree =
            match t with
            | Node n when n.v = v -> n.r
            | Node n when n.v > v -> Node { n with l = replace_successor n.l v}
            | Node n when n.v <= v -> Node { n with r = replace_successor n.r v}
            | _ -> raise (BadTraversal "Successor not found in tree")

        let rec remove (t : 'a bstree) (v : 'a) : 'a bstree =
            match t with
            | Node { v = value; l = Empty; r = Empty} when value = v -> Empty (* Remove leaf *)
            | Node n when n.v > v -> Node { n with l = remove n.l v } (* Remove from left *)
            | Node n when n.v < v -> Node { n with r = remove n.r v } (* Remove from right *)
            | Node { v = value; l = left; r = Empty } when value = v -> left (* Replace with child *)
            | Node { v = value; l = Empty; r = right } when value = v -> right (* Replace with child *)
            | Node { v = value; l = left; r = right } when value = v -> (
                    let successor = minimum right in
                    match successor with
                    | Empty -> raise (BadTraversal "No successor")
                    | Node s ->(
                            match right with
                            | Empty -> raise (BadTraversal "Right not found")
                            | Node r -> if s.v = r.v then
                                Node { s with l = left }
                            else
                                let new_tree = replace_successor successor s.v in
                                match new_tree with
                                | Empty -> raise (BadTraversal "No successor tree")
                                | Node n -> Node {n with l = left; r = right}
                            )) 
            | _ -> raise (NotMem "Node to delete not found")

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

let %expect_test "test-print_bstree-empty" =
    let open BinarySearchTree in
    let tree = Empty in
    print_bstree tree;
    [%expect {|  |}]

let %expect_test "test-print_bstree-root" =
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 1 in
    print_bstree tree;
    [%expect {| 1. |}]

let %expect_test "test-print_bstree-l_child" =
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = add tree 1 in
    print_bstree tree;
    [%expect {| 10{1.} |}]

let %expect_test "test-print_bstree-r_child" =
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = add tree 100 in
    print_bstree tree;
    [%expect {| 10[100.] |}]

let %expect_test "test-print_bstree-two_children" =
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = add tree 100 in
    let tree = add tree 1 in
    print_bstree tree;
    [%expect {| 10{1.}[100.] |}]

let %expect_test "test-print_bstree-two_generations" =
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = add tree 100 in
    let tree = add tree 5 in
    let tree = add tree 1 in
    let tree = add tree 7 in
    print_bstree tree;
    [%expect {| 10{5{1.}[7.]}[100.] |}]

let %expect_test "test-print_bstree-two_generations_full" =
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = add tree 100 in
    let tree = add tree 5 in
    let tree = add tree 1 in
    let tree = add tree 7 in
    let tree = add tree 50 in
    let tree = add tree 150 in
    print_bstree tree;
    [%expect {| 10{5{1.}[7.]}[100{50.}[150.]] |}]

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

let %expect_test "test-remove-empty" = 
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = remove tree 10 in
    print_string (string_of_bool(tree = Empty));
    [%expect {| true |}]

let %expect_test "test-remove-left" = 
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = add tree 1 in
    let tree = remove tree 1 in
    match tree with
    | Node n ->
            print_int n.v;
            [%expect {| 10 |}];
            print_string (string_of_bool (n.l = Empty));
            [%expect {| true |}]
    | _ -> [%expect.unreachable]

let %expect_test "test-remove-right" = 
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = add tree 100 in
    let tree = remove tree 100 in
    match tree with
    | Node n ->
            print_int n.v;
            [%expect {| 10 |}];
            print_string (string_of_bool (n.r = Empty));
            [%expect {| true |}]
    | _ -> [%expect.unreachable]

let %expect_test "test-remove-parent_left" = 
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = add tree 1 in
    let tree = remove tree 10 in
    match tree with
    | Node n ->
            print_int n.v;
            [%expect {| 1 |}];
            print_string (string_of_bool (n.l = Empty));
            [%expect {| true |}]
    | _ -> [%expect.unreachable]

let %expect_test "test-remove-parent_right" = 
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 10 in
    let tree = add tree 100 in
    let tree = remove tree 10 in
    match tree with
    | Node n ->
            print_int n.v;
            [%expect {| 100 |}];
            print_string (string_of_bool (n.r = Empty));
            [%expect {| true |}]
    | _ -> [%expect.unreachable]

let %expect_test "test-remove-full_two_gen_leaf" =
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 5 in
    let tree = add tree 3 in
    let tree = add tree 2 in
    let tree = add tree 4 in
    let tree = add tree 7 in
    let tree = add tree 6 in
    let tree = add tree 8 in
    let tree = remove tree 2 in
    print_bstree tree;
    [%expect {| 5{3[4.]}[7{6.}[8.]] |}]

let %expect_test "test-remove-full_two_gen_imm_successor" =
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 5 in
    let tree = add tree 3 in
    let tree = add tree 2 in
    let tree = add tree 4 in
    let tree = add tree 7 in
    let tree = add tree 6 in
    let tree = add tree 8 in
    let tree = remove tree 3 in
    print_bstree tree;
    [%expect {| 5{4{2.}}[7{6.}[8.]] |}]

let %expect_test "test-remove-full_two_gen_nonimm_successor" =
    let open BinarySearchTree in
    let tree = Empty in
    let tree = add tree 5 in
    let tree = add tree 3 in
    let tree = add tree 2 in
    let tree = add tree 4 in
    let tree = add tree 8 in
    let tree = add tree 6 in
    let tree = add tree 9 in
    let tree = add tree 7 in
    let tree = remove tree 5 in
    print_bstree tree;
    [%expect {| 6{3{2.}[4.]}[8{7.}[9.]] |}]
