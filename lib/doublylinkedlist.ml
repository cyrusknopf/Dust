(* Imperative, mutable solution which hides behind an immutable interface *)
(* Used this approach to avoid complications with cyclic datastructures in purely functional *)
module DLList : sig
    type 'a node =
        | Nil
        | Node of {
            v : 'a;
            mutable n : 'a node;
            mutable p : 'a node 
        }

    val create : 'a -> 'a node
    val hd : 'a node -> 'a node
    val tl : 'a node -> 'a node
    val value : 'a node -> 'a
    val print_dll : int node -> unit
    val cons : 'a node -> 'a -> 'a node

end = struct
    type 'a node =
        | Nil
        | Node of {
            v : 'a;
            mutable n : 'a node;
            mutable p : 'a node
        }

    exception NilValue of string
    (* exception Empty of string *)

    let rec print_dll (l : int node) =
        match l with
        | Nil -> print_string ""
        | Node n -> print_int n.v;
        match n.n with
        | Nil -> print_string ""
        | Node _ -> print_string "<->"; print_dll n.n

    let create (v : 'a) : 'a node =
        Node {v; n=Nil; p=Nil}

    let hd (l : 'a node) : 'a node =
        match l with
        | Nil -> Nil
        | Node h -> Node { h with n = Nil}

    let tl (l : 'a node) : 'a node =
        match l with
        | Nil -> Nil
        | Node n -> match n.n with
            | Nil -> Nil
            | Node next -> Node { next with p = Nil }

    let value (n : 'a node) : 'a =
        match n with
        | Nil -> raise (NilValue "Node is nil-valued")
        | Node n -> n.v

    (* Imperative solution, but hidden behind immutable interface *)
    let cons (l : 'a node) (v : 'a) : 'a node =
        match l with
        | Nil -> Node { v; n = Nil; p = Nil }
        | Node n ->
                let new_head = Node { v; n = l; p = Nil } in
                n.p <- new_head; new_head


end
include DLList
