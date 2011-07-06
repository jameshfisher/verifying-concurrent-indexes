module rb.node;

struct Node {
  int value;
  Node*[2] c;
  bool black;

  this(int value) {
    this.value = value;
    this.c[0] = this.c[1] = null;
    this.black = false;
  }
}

/*
We need to define some predicates to describe the state of the tree
at various points in the procedures -- types of valid and invalid trees and subtrees.

First, a simple predicate on how a tree represents the set by ordering it:

  Compose(L, v, R, S) =         // The set L, value v, and set R
    L ∪ {v} ∪ R == S    &&      // make up the set S, and
    (∀ w∈L. w < value)  &&      // all values in L are less than v, and
    (∀ w∈R. w > value)          // all values in R are greater than v.

It is convenient to think of a Red-Black tree as composed of red-rooted trees and black-rooted trees.
These I refer to respectively as RT and BT.

  RT(n, S, bh) =                // a red-rooted tree pointed to by `n` represents set S with a tree of black-height `bh` if
    ∃ v, c[0], c[1], L, R.      //    there exist element `v` and sets L and R
    Compose(L, v, R, S) &&      //    such that L, v and R make up S, and
    n↦v,c[0],c[1],false ∗       //    n points to a valid red node (i.e., black==false) containing `v` and child pointers
    BT(c[0], L, bh) ∗           //    to black trees representing L
    BT(c[1], R, bh).            //    and R, with the same black height (as the red node takes up no height).

There's only one kind of RT, as the rules demand both children are black.
However, there are multiple types of BT, analogous to the multiple node types in a 2,3,4-tree.
There are two-nodes, in which both children are also black.
There are three-nodes, in which one child is black (a BT) and one is red (an RT).
That is, the RBT rules have two representations of a three-node:
where the left child is the RT ("left-leaning"),
and where the right child is the RT ("right-leaning").
Finally, there are four-nodes, where both children are RTs.

First, two-nodes (2T).
Here, we capture that the empty set is represented by the null pointer
(the null pointer is a 2T and only a 2T).

  2T(n, S, bh) =                // a two-node pointed to by `n` represents set S with black-height `bh` if
      n == NULL &&              //    `n` is null, in which case
      S == {} &&                //    it represents the empty set,
      emp &&                    //    the heap is empty,
      bh == 1                   //    and we have a black-height of 1;
    ||                          // or `n` is not null, so
      ∃ v, c[0], c[1], L, R.    //    there exist element `v` and sets L and R
      Compose(L, v, R, S) &&    //    such that L, v and R make up S, and
      n↦v,c[0],c[1],true ∗      //    `n` points to (implies not null) a valid black node (i.e., black==false) containing `v` and child pointers
      BT(c[0], L, bh-1) ∗       //    to black trees representing L and R,
      BT(c[1], R, bh-1).        //    with one less black height (as they do not contain this black node).

Next, three-nodes.
We could create predicates LeftLeaning3T and RightLeaning3T,
but then we lose the benefit of parameterizing the link direction.
So we parameterize the predicate:

  3T(n, S, bh, 0) =             // a left-leaning 3T
    ∃ v, c[0], c[1], L, R.
    Compose(L, v, R, S) &&
    n↦v,c[0],c[1],true ∗
    RT(c[0], L, bh-1) ∗
    BT(c[1], R, bh-1).

  3T(n, S, bh, 1) =             // a right-leaning 3T
    ∃ v, c[0], c[1], L, R.
    Compose(L, v, R, S) &&
    n↦v,c[0],c[1],true ∗
    BT(c[0], L, bh-1) ∗
    RT(c[1], R, bh-1).

A general three-node is simply one that's left-leaning or right-leaning:

  3T(n, S, bh) =
    3T(n, S, bh, 0) ||
    3T(n, S, bh, 1).

Four-nodes are easier:

  4T(n, S, bh, 0) =
    ∃ v, c[0], c[1], L, R.
    Compose(L, v, R, S) &&
    n↦v,c[0],c[1],true ∗
    RT(c[0], L, bh-1) ∗
    RT(c[1], R, bh-1).

We define a general black tree (BT) as one of the valid pseudo-node types:

  BT(n, S, bh) =
    2T(n, S, bh) ||
    3T(n, S, bh) ||
    4T(n, S, bh).

We wrap things up with a separate RBT predicate:

  RBT(n, S) =
    ∃ bh. BT(n, S, bh).

The above predicates describe valid parts of an RBT.
Through the procedures, however, we have trees that are invalid, but have some guarantees.
A red-violated tree is one with a red root and one RT as a child.
As with the three-node, it swings both ways, so requires parameterization:

  RVT(n, S, bh, 0) =        // left-leaning red-violated tree
    ∃ v, c[0], c[1], L, R.
    Compose(L, v, R, S) &&
    n↦v,c[0],c[1],false ∗
    RT(c[0], L, bh) ∗
    BT(c[1], R, bh).

  RVT(n, S, bh, 1) =        // right-leaning red-violated tree
    ∃ v, c[0], c[1], L, R.
    Compose(L, v, R, S) &&
    n↦v,c[0],c[1],false ∗
    BT(c[0], L, bh) ∗
    RT(c[1], R, bh).
*/


bool red(Node * node) {
  return node != null && node.black == false;
}

