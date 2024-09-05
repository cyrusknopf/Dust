open! Dust.Doublylinkedlist

let %expect_test "test-create-nil" =
    let l = create 1 in
    print_dll l;
    [%expect {| 1. |}]
