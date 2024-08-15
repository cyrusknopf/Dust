open Dust.Max_heap

let test_init () =
    let mh = MaxHeap.init () in
    match mh.root with
    | None -> ()
    | _ -> Alcotest.fail "Root not None on init"

let test_heapify () =
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




let suite =
  [
      "Test init", `Quick, test_init;
      "Test heapify", `Quick, test_heapify
  ]

let () =
  Alcotest.run "Priority Queue" [ "Max Heap", suite ]
