# Dust

Functional, immutable data structures implemented in OCaml with the standard library

Using `ppx_expect` for checking correctness

The library contains implementations of ...

- Zipper
> An implementation of [Ariadne's Zipper](https://en.wikibooks.org/wiki/Haskell/Zippers) which uses the toy example of a labyrinth with dead ends, forwards passages, and forks with left and right paths to motivate zippers.

- Binary Search Tree
> A vanilla, non-balancing binary tree, where the value of each node is lesser or equal to the value of its right child, and lesser than the value of its left child. Signature provided for a generic tree, implemented specifically for types which use the `<` and `>` operators.

- Double Ended Queue (Dequeue)
> "FIFO" data structure which supports insertion and removal at both ends, courtesy of 
<a href="https://www.cs.cmu.edu/~rwh/students/okasaki.pdf" target="_blank">Okasaki</a>. Implemented using linked lists from the OCaml standard library.

- Doubly Linked List
> Linked list where each node has `next` and `previous` pointers. Implemented using minimal imperative features of OCaml, hidden behind an immutable interface.
