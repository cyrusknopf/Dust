open Core_bench
open Core

open! Dust.Bstree

let rec insert_btree (t : 'a bstree) ( n : int) =
    if n <= 0 then t
    else
        let t' = BSTree.add t n in
    insert_btree t' (n-1)


let () =
    let bench = Bench.Test.create ~name:"bstree_create" (fun () -> ignore (BSTree.create ())) in
    Bench.make_command [bench] |> Command_unix.run;

    let bench = Bench.Test.create ~name:"bstree_insert_100" (fun () ->
        let t = BSTree.create () in
        let _ = insert_btree t 100 in
        ignore t
        )
    in
    Bench.make_command [bench] |> Command_unix.run
