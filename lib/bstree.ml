module BSTree : sig
        type 'a bstree = 
            | Empty
            | Node of {
                v : 'a;             (* Value of node *)
                l : 'a bstree;      (* Left child of node *)
                r : 'a bstree;      (* Right child of node *)
            }
        val create : unit -> 'a bstree
        val height : 'a bstree -> int
        val print_bstree : int bstree -> unit
        val add : 'a bstree -> 'a -> 'a bstree
        val remove : 'a bstree -> 'a -> 'a bstree
        val find : 'a bstree -> 'a -> bool

    end = struct
        (* Binary search tree implementation *)
        (* Values are inserted to the right if they are equal *)
        type 'a bstree =
            | Empty                 (* Empty node *)

            | Node of {             (* Node with one or two children *)
                v : 'a;             (* Value of node *)
                l : 'a bstree;      (* Left child of node *)
                r : 'a bstree;      (* Right child of node *)
            }

        exception NotMem of string
        exception BadTraversal of string

        let create () = Empty

        let rec height (t : 'a bstree) : int =
            match t with
            | Node n -> 1 + max (height n.l) (height n.r)
            | Empty -> 0

        (* Linearly prints a tree with a node's left child in {} and right in [] *)
        (* Leaves are followed by a . *)
        let rec print_bstree tree : unit =
            match tree with
            | Empty -> print_string ""
            | Node {v=v; l=Empty; r=Empty} -> print_int v; print_string "."
            | Node {v=v; l=l;r=Empty} -> print_int v; 
            print_string "{"; print_bstree l; print_string "}"
            | Node {v=v; l=Empty;r=r} -> print_int v; 
            print_string "["; print_bstree r; print_string "]"
            | Node n -> print_int n.v;
            print_string "{"; print_bstree n.l; print_string "}";
            print_string "["; print_bstree n.r; print_string "]"

            (*
        let rec maximum (t : 'a bstree) : 'a bstree =
            match t with
            | Node { r = right; _} when right <> Empty -> maximum right
            | _ -> t
            *)

        let rec minimum (t : 'a bstree) : 'a bstree =
            match t with
            | Node { l = left; _} when left <> Empty -> minimum left
            | _ -> t

        (* Insert a node into the tree *)
        let rec add (t : 'a bstree) (v : 'a) : 'a bstree =
            match t with
            | Empty -> Node { v; l = Empty; r = Empty }
            | Node { v = value; l = Empty; r = Empty } ->
                    let child = Node { v; l = Empty; r = Empty } in
                    if value > v then
                        Node { v = value; l = child; r = Empty }
                    else if value <= v then
                        Node { v = value; l = Empty; r = child}
                    else
                        raise (BadTraversal "Failed to traverse when adding")

            (* Create new subtree with it added to the left *)
            | Node n when n.v > v -> Node { n with l = add n.l v }

            (* Create new subtree with it added to the right *)
            | Node n when n.v <= v -> Node { n with r = add n.r v }
            | _ -> raise (BadTraversal "Failed to traverse node when adding")

        let rec replace_successor (t : 'a bstree) (v : 'a) : 'a bstree =
            match t with
            | Node n when n.v = v -> n.r
            | Node n when n.v > v -> Node { n with l = replace_successor n.l v}
            | Node n when n.v <= v -> Node { n with r = replace_successor n.r v}
            | _ -> raise (BadTraversal "Successor not found in tree")

        (*
        Each case is based on the cases proposed here:
        https://en.wikipedia.org/wiki/Binary_search_tree#Deletion
        However, the provided successor algorithm is not used, since in the final deletion
        case there will always be a right child of the node to find the successor of
        *)
        let rec remove (t : 'a bstree) (v : 'a) : 'a bstree =
            match t with
            | Node { v = value; l = Empty; r = Empty} when value = v -> Empty (* Remove leaf *)
            | Node n when n.v > v -> Node { n with l = remove n.l v } (* Remove from left *)
            | Node n when n.v < v -> Node { n with r = remove n.r v } (* Remove from right *)
            | Node { v = value; l = left; r = Empty } when value = v -> left (* Replace with child *)
            | Node { v = value; l = Empty; r = right } when value = v -> right (* Replace with child *)
            | Node { v = value; l = left; r = right } when value = v -> ( (* Node to delete has two children *)
                    let successor = minimum right in
                    match successor with
                    | Empty -> raise (BadTraversal "No successor")
                    | Node s ->(
                            match right with
                            | Empty -> raise (BadTraversal "Right not found")
                            | Node r -> 
                                if s.v = r.v then (* If successor is the right child, replace with successor (d) *)
                                    Node { s with l = left }
                                else (* Else, displace with successor (e) *)
                                    let new_tree = replace_successor right s.v in
                                    match new_tree with
                                    | Empty -> raise (BadTraversal "No successor tree")
                                    | Node _ -> Node { v = s.v; l = left; r = new_tree }
                            )) 
            | _ -> raise (NotMem "Node to delete not found")

        (* Determine whether a node is in the tree *)
        let rec find (t : 'a bstree) (v : 'a) : bool =
            match t with
            | Empty -> false
            (* Leaf whose value is not the one we are looking for -> false *)
            | Node { v = value; l  = Empty; r = Empty } when value <> v -> false
            (* Otherwise, if leaf then must have correct value -> true *)
            | Node { v = _; l  = Empty; r = Empty} -> true
            | Node { v = value; _} when value = v -> true (* If node val = v *)
            | Node n when n.v > v -> (* If node value greater, search left *)
                    find n.l v
            | Node n -> (* If node value lesser or equal, search right *)
                    find n.r v
    end
include BSTree
