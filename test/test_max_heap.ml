open Dust.Max_heap

let test_init () =
    let mh = MaxHeap.init () in
    match mh.root with
    | None -> ()
    | _ -> Alcotest.fail "Root not None on init"

let test_heapify_l_child_only () =
    let ch1 = { MaxHeap.value = "a"; prio = 1; l_child = None; r_child = None } in
    let r = { MaxHeap.value = "b"; prio = 0; l_child = Some ch1; r_child = None } in
    let mh = { MaxHeap.root = Some r } in
    let mh' = MaxHeap.heapify mh in
    match mh'.root with
    | Some r when r.value = "a" -> (
        match r.l_child, r.r_child with
        | Some left, None when left.value = "b" -> ()
        | _ -> Alcotest.fail "Expected root.l_child to have value 'b'")
    | _ -> Alcotest.fail "Expected mh'.root to have value 'a'"

let test_heapify_r_child_only () =
    let ch1 = { MaxHeap.value = "a"; prio = 1; l_child = None; r_child = None } in
    let r = { MaxHeap.value = "b"; prio = 0; l_child = None; r_child = Some ch1 } in
    let mh = { MaxHeap.root = Some r } in
    let mh' = MaxHeap.heapify mh in
    match mh'.root with
    | Some r when r.value = "a" -> (
        match r.l_child, r.r_child with
        | None, Some right when right.value = "b" -> ()
        | _ -> Alcotest.fail "Expected root.l_child to have value 'b'")
    | _ -> Alcotest.fail "Expected mh'.root to have value 'a'"

let test_heapify_two_children () =
    let ch1 = { MaxHeap.value = "a"; prio = 1; l_child = None; r_child = None } in
    let ch2 = { MaxHeap.value = "b"; prio = 2; l_child = None; r_child = None } in
    let r = { MaxHeap.value = "c"; prio = 0; l_child = Some ch1; r_child = Some ch2 } in
    let mh = { MaxHeap.root = Some r } in
    let mh' = MaxHeap.heapify mh in
    match mh'.root with
    | Some r when r.value = "b" -> (
        match r.l_child, r.r_child with
        | Some left, Some right when left.value = "a" && right.value = "c" -> ()
        | _ -> Alcotest.fail "Expected left child to remain the same and right to be root")
    | _ -> Alcotest.fail "Expected root to be 'b'"


let suite =
  [
      "Test init", `Quick, test_init;
      "Test heapify: left child only", `Quick, test_heapify_l_child_only;
      "Test heapify: right child only", `Quick, test_heapify_r_child_only;
      "Test heapify: two children", `Quick, test_heapify_two_children;

  ]

let () =
  Alcotest.run "Priority Queue" [ "Max Heap", suite ]
