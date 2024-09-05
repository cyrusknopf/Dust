module DLinkedList : sig
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
        | Nil -> print_string "."
        | Node _ -> print_string "⇄"; print_dll n.n

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

    (* let cons (l : 'a node) (v : 'a) : 'a node = l *)

end
include DLinkedList
