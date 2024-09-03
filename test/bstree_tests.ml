open Dust.Bstree.BSTree

(* TESTS *)
let %expect_test "test-add-empty" = 
    let tree = create () in
    let tree = add tree 10 in
    print_bstree tree;
    [%expect {| 10. |}]

let %expect_test "test-add-child_lesser" =
    let tree = create () in
    let tree = add tree 10 in
    let tree = add tree 1 in
    print_bstree tree;
    [%expect {| 10{1.} |}]


let %expect_test "test-add-child_greater" =
    let tree = create () in
    let tree = add tree 10 in
    let tree = add tree 11 in
    print_bstree tree;
    [%expect {| 10[11.] |}]

let %expect_test "test-print_bstree-two_children" =
    let tree = create () in
    let tree = add tree 10 in
    let tree = add tree 100 in
    let tree = add tree 1 in
    print_bstree tree;
    [%expect {| 10{1.}[100.] |}]

let %expect_test "test-print_bstree-two_generations" =
    let tree = create () in
    let tree = add tree 10 in
    let tree = add tree 100 in
    let tree = add tree 5 in
    let tree = add tree 1 in
    let tree = add tree 7 in
    print_bstree tree;
    [%expect {| 10{5{1.}[7.]}[100.] |}]

let %expect_test "test-print_bstree-two_generations_full" =
    let tree = create () in
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
    let tree = create() in
    let tree = add tree 10 in
    let mem = find tree 10 in
    print_string (string_of_bool mem);
    [%expect {| true |}];
    let mem = find tree 1 in
    print_string (string_of_bool mem);
    [%expect {| false |}]

let %expect_test "test-find-root" = 
    let tree = create () in
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
    let tree = create () in
    let tree = add tree 10 in
    let tree = remove tree 10 in
    print_bstree tree;
    [%expect {| |}]

let %expect_test "test-remove-left" = 
    let tree = create () in
    let tree = add tree 10 in
    let tree = add tree 1 in
    let tree = remove tree 1 in
    print_bstree tree;
    [%expect {| 10. |}]

let %expect_test "test-remove-right" = 
    let tree = create () in
    let tree = add tree 10 in
    let tree = add tree 100 in
    let tree = remove tree 100 in
    print_bstree tree;
    [%expect {| 10. |}]

let %expect_test "test-remove-parent_left" = 
    let tree = create () in
    let tree = add tree 10 in
    let tree = add tree 1 in
    let tree = remove tree 10 in
    print_bstree tree;
    [%expect {| 1. |}]

let %expect_test "test-remove-parent_right" = 
    let tree = create () in
    let tree = add tree 10 in
    let tree = add tree 100 in
    let tree = remove tree 10 in
    print_bstree tree;
    [%expect {| 100. |}]

let %expect_test "test-remove-full_two_gen_leaf" =
    let tree = create () in
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
    let tree = create () in
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
    let tree = create () in
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
