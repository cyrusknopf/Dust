open Dust.Priority_queue


let test_init () =
    let mh = MaxHeap.init () in
    match mh.root with
    | None -> ()
    | _ -> Alcotest.fail "Root not None on init"



let suite =
  [
      "Test init", `Quick, test_init;
  ]

let () =
  Alcotest.run "Priority Queue" [ "Max Heap", suite ]
