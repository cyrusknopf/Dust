open Dust.Doublylinkedlist

let %expect_test "test-create-nil" =
    let l = create 1 in
    print_dll l;
    [%expect {| 1 |}]

let %expect_test "test-cons-singleton" = 
    let l = create 1 in
    let l' = cons l 2 in
    print_dll l';
    [%expect {| 2<->1 |}]

let %expect_test "test-cons-two" = 
    let l = create 1 in
    let l = cons l 2 in
    let l = cons l 3 in
    print_dll l;
    [%expect {| 3<->2<->1 |}]

let %expect_test "test-tl-three" = 
    let l = create 1 in
    let l = cons l 2 in
    let l = cons l 3 in
    print_dll (tl l);
    [%expect {| 2<->1 |}]
