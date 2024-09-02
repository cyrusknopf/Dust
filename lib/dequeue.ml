module Dequeue =
    struct
        (* Forward front list, reversed back list from Okasaki - Functional Data Structures *)
        type 'a dequeue = 'a list * 'a list

        exception Empty

        let create () : 'a dequeue = ([],[])

        (* Simply cons of front list: O(1) *)
        let prepend (d : 'a dequeue) (x : 'a) : 'a dequeue =
            match d with
            | (xs, ys) -> (x::xs, ys)

        (* Simply cons of back list: O(1) *)
        let append (d : 'a dequeue) (y : 'a) : 'a dequeue =
            match d with
            | (xs, ys) -> (xs, y::ys)

        (* Worst case: O(n) + O(1) = O(n) in reverse case *)
        (* Average O(1) as front list is rarely empty *)
        let hd (d : 'a dequeue) : 'a =
            match d with
            | ([], []) -> raise Empty
            | ([], ys) -> let rev = List.rev ys in List.hd rev
            | (xs, _) -> List.hd xs

        (* Worst case: O(n) + O(1) = O(n) in reverse case *)
        (* Average O(1) as back list is rarely empty *)
        let tl (d : 'a dequeue) : 'a =
            match d with
            | ([], []) -> raise Empty
            | (xs, []) -> let rev = List.rev xs in List.hd rev
            | (_, ys) -> List.hd ys
    end
