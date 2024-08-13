open Dust.Max_heap


let test_init () =
    let mh = MaxHeap.init () in
    match mh.root with
    | None -> ()
    | _ -> Alcotest.fail "Root not None on init"

let test_empty_enque () =
    let mh = MaxHeap.init () in
        let node = MaxHeap.create_node "a" 1 in
            let mh' = MaxHeap.push mh node in
                match mh'.root with
                | Some node when node.data = "a" -> ()
                | _ -> Alcotest.fail "Root not enqueued node"

let test_root_larger () = 
    let mh = MaxHeap.init () in 
    let node1 = MaxHeap.create_node "a" 10 in
    let mh' = MaxHeap.push mh node1 in
    let node2 = MaxHeap.create_node "b" 1 in
    let mh'' = MaxHeap.push mh' node2 in
    match mh''.root with
    | Some node when node.data = "a" ->
            (match node.l_child with
            | Some no when no.data = "b" -> ()
            | _ -> Alcotest.fail "Lchild not B")
    | _ -> Alcotest.fail "Root not larger value"

let test_root_smaller () = 
    let mh = MaxHeap.init () in 
    let node1 = MaxHeap.create_node "a" 1 in
    let mh' = MaxHeap.push mh node1 in
    let node2 = MaxHeap.create_node "b" 10 in
    let mh'' = MaxHeap.push mh' node2 in
    match mh''.root with
    | Some node when node.data = "a" -> (
            match node.r_child with
            | Some no when no.data = "b" -> ()
            | _ -> Alcotest.fail "Lchild not B"
    )
    | _ -> Alcotest.fail "Root not larger value"

let test_layer_1_full () =
    let mh = MaxHeap.init () in 
    let node1 = MaxHeap.create_node "a" 5 in
    let mh' = MaxHeap.push mh node1 in
    let node2 = MaxHeap.create_node "b" 10 in
    let mh'' = MaxHeap.push mh' node2 in
    let node3 = MaxHeap.create_node "c" 1 in
    let mh''' = MaxHeap.push mh'' node3 in 
    let node4 = MaxHeap.create_node "d" 3 in
    let mh'''' = MaxHeap.push mh''' node4 in
    match mh''''.root with
    | Some node -> (
        match node.l_child with
            | Some onode -> (
                match onode.r_child with
                | Some enode when enode.data = "d" -> ()
                | _ -> Alcotest.fail ""
        )
            | _ -> Alcotest.fail ""
    )
    | _ -> Alcotest.fail "Incorrect"




let suite =
  [
      "Test init", `Quick, test_init;
      "Test enqueue empty", `Quick, test_empty_enque;
      "Test enqueue root greater", `Quick, test_root_larger;
      "Test enqueue root lesser ", `Quick, test_root_smaller;
      "Test layer 1 full", `Quick, test_layer_1_full;
  ]

let () =
  Alcotest.run "Priority Queue" [ "Max Heap", suite ]
