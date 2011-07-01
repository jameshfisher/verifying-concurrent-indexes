#!/usr/bin/rdmd -debug

import std.c.stdio;
import std.stdio;
import std.conv;
import std.random;

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


Node * rotate_single(Node * oldRoot, int dir) {

  // 3T(n, S, bh, dir)

  //        oldRoot, 3T(H, dir)
  //         /           \
  //        /             \
  //       /         newRoot, RT(dir, H-1)
  //      /               /       \
  //     /               /         \
  // sibling,       gc1,          gc2, 
  // BT(H-1)        BT(H-1)       BT(H-1)

  // Open predicates:
  //
  //          oldRoot, B
  //         /           \
  //        /             \
  //       /             newRoot, R
  //      /               /       \
  //     /               /         \
  // sibling,       gc1,          gc2, 
  // BT(H-1)        BT(H-1)       BT(H-1)

  Node * newRoot = oldRoot.c[!dir];
  oldRoot.c[!dir] = newRoot.c[dir];
  newRoot.c[dir] = oldRoot;

  newRoot.black = true;
  oldRoot.black = false;

  //            newRoot, B
  //             /          \
  //            /            \
  //     oldRoot, R           \
  //      /      \             \
  //     /        \             \
  // sibling,       gc1,          gc2, 
  // BT(H-1)        BT(H-1)       BT(H-1)

  // Close predicates:
  //
  //            newRoot, 3T(H, !dir)
  //             /          \
  //            /            \
  //   oldRoot, RT(H-1)       \
  //      /      \             \
  //     /        \             \
  // sibling,       gc1,          gc2, 
  // BT(H-1)        BT(H-1)       BT(H-1)

  // 3T(n, S, bh, !dir)

  return newRoot;
}

void rotate_single_ptr(Node ** oldRoot, int dir) {
  *oldRoot = rotate_single(*oldRoot, dir);
}


Node * rotate_double(Node * oldRoot, int dir) {

  // n↦v,c[0],c[1],?  ∗  BT(c[dir], L, h-1)  ∗  RVT(c[!dir], R, h-1, !dir)  && Compose(L, v, R, S)

  // rotate_double can be used to fix a red violation.
  // An analogy here is rearranging the elements of a four-node:
  //
  //      |                             |
  //      a---b---c     ==>         a---b---c
  //    /   |   |   \             /   |   |   \

  //          <=====dir======
  //    
  //             oldRoot, R/B
  //           /           \
  //          /             \
  //         /          child, RVT(!dir, h-1)
  //        /               /               \
  //       /               /                 \
  //      /          newRoot, RT(h-1)         \
  //     /           /             \           \
  //    /           /               \           \
  //  sib,        ggc1,           ggc2,         gc2,
  //  B(h-1)      BT(h-1)         BT(h-1)       BT(h-1)

  oldRoot.c[!dir] = rotate_single(oldRoot.c[!dir], !dir);
  Node * newRoot = rotate_single(oldRoot, dir);

  // 4T(n, S, h)

  //              ---newRoot, 4T(h)---
  //             /                    \
  //            /                      \
  //     oldRoot, RT(h-1)           child, RT(h-1)
  //     /         \                 /         \
  //    /           \               /           \
  //  sib,        ggc1,           ggc2,         gc2,
  //  B(h-1)      BT(h-1)         BT(h-1)       BT(h-1)

  return newRoot;
}

void rotate_double_ptr(Node ** oldRoot, int dir) {
  *oldRoot = rotate_double(*oldRoot, dir);
}


bool search_rec(Node * root, int value) {
  if (root == null) return false;
  if (root.value == value) return true;
  return search_rec(root.c[root.value < value], value);
}


Node * insert_rec(Node * root, int value) {

  Node * o;
  // We are going to return o.

  if (root == null) {
    // insertion into the empty set is just a red node.
    o = new Node(value);
  }
  else if (value == root.value) {
    // the tree already contains the value. Already a valid tree. No adjustment required.
    o = root;
  }
  else {
    // We have to insert value into a subtree.

    int dir = root.value < value;  // We need to insert in that subtree.

    Node ** into = &root.c[dir];
    *into = insert_rec(*into, value); // Do the insert.

    // Now, insert may have passed up a red node.
    // If we have room, make a home for it here; otherwise pass up again.

    if (red(*into)) {
      // A red node was passed up.  Make room, or pass it up again.

      Node ** sibling = &root.c[!dir];

      if (red(*sibling)) {
        // The sibling is red at height N-1.  Implies root is black, at height N.

        (*sibling).black = true;  // Turn sibling into black tree (height N).
        (*into).black = true;     // Set passed-up node black.  If it passed by an RV'd tree, that's fixed by it.  Is now BT(N).
        root.black = false;       // Then turn `root` into red tree at height N.  We've pushed the red up the tree.
      }
      else {
        // The sibling is black.  root maybe red.
        Node ** far_child = &(*into).c[dir];
        if (red(*far_child)) {
          // into is red, far child is red.  RV.  sibling is black.

          //         root, R/B  <== was previously either BT(N) or RT(N-1)
          //       /           \
          //      /             \
          // sibling, B(N-1)   into, RVT(dir, N-1)
          //                    /       \
          //                   /         \
          //     near child, B(TN-1)    far child, R(N-1)

          rotate_single_ptr(&root, !dir);

          //            --into, B(N)--
          //           /              \
          //          /                \
          //        root, R(N-1)    far child, R(N-1)
          //       /           \
          //      /             \
          //  sibling, B(N-1)   near child, BT(N-1)

          // Rebalancing stops here.
        }
        else {
          // far child is black.
          Node ** near_child = &(*into).c[!dir];
          if (red(*near_child)) {
            // near child is red.

            //         root, R/B  <== was previously either BT(N) or RT(N-1)
            //       /           \
            //      /             \
            // sibling, B(N-1)   into, RVT(!dir, N-1)
            //                    /           \
            //                   /             \
            //        near_child, RT(N-1)     far child, BT(N-1)
            //          /             \
            //         /               \
            //  near_gc, BT(N-1)  far_gc, BT(N-1)

            rotate_double_ptr(&root, !dir);

            //            near child, BT(N)
            //           /                \
            //          /                  \
            //     root, RT(N-1)        into, RT(N-1)
            //    /         \            /       \
            // sibling,   near gc,    far gc,     far child,
            // BT(N-1)    BT(N-1)     BT(N-1)     BT(N-1)

            // Rebalancing stops here.
          }
          else {
            // near child is black.

            //         root, RVT(dir, N-1) or BT(N)
            //       /           \
            //      /             \
            // sibling, B(N-1)   into, RT(N-1)
            //                    /           \
            //                   /             \
            //        near_child, BT(N-1)     far child, BT(N-1)

            // Either root is black, and rebalancing stops here, or root is red, and we have an RVT to pass up.
          }
        }
      }
    }
    else {
      // No red was passed up.  We're OK.
    }

    o = root;
  }

  return o;
}


void remove_push_down_iter(Tree * tree, int value) {
  if (tree.root == null) return;                // The easy case: deletion from {} is {}, with no balancing.

  auto superroot = new Node(0); superroot.black = true;   // A false tree root.  Simplifies cases.
  superroot.c[1] = tree.root;  int dir = 1;  // The false root has the real root as its right child.

  Node * found = null;  // If we find the value on the way down, and it's not a leaf, we put the node here.
  Node * node = superroot;                      // `node` will descend the tree to a leaf.
  Node * parent = null, grandparent = null;     // The parent and grandparent of `node`.

  // We'll continue down the tree, after which `node` will be the dummy to remove the value from.
  // Then if `found` is null, we can just remove `node`.  Otherwise, we move `node`'s value into `found`, then remove node.

  // We ensure that before descending one level, we're not descending to a pseudo-node.

  while (node.c[dir] != null) {
    // `dir` is the direction we need to go in next.
    // node != null.  node.c[dir] != null.  Unless on first iteration, node.c[dir] is guaranteed not an empty pseudonode.
    // We're going to descend to node.c[dir], then ensure we can descend again.

    // Start by descending one node to node.c[dir].
    // The previous iteration sorted out that this is OK to do;
    // unless there was no such previous iteration, in which case
    // we started with `node` as the fake superroot,
    // so the first real `node` we consider is the root.  Which might be an empty pseudonode.

    // We have at least one iteration of this loop.

    int last = dir;  // Store whether this iteration's `node` is a left or right child.

    // Descend.
    grandparent = parent;
    parent = node;
    node = node.c[dir];

    // Now `node` is "this iteration's node".
    // node != null.
    // parent != null.  Might be the superroot.
    // `node` is a non-empty pseudonode, or it is the root, which may be empty pnode.

    dir = node.value < value; // Value is not in node.c[!dir].

    // save found node
    if (node.value == value) {
      // value is here.  Its in-order predecessor, if it has one, is in node.c[0].
      found = node;
    }

    // `found` might be null.
    // If found == null, we did not set it, it is not in node, and it is not in node.c[!dir], so if it is in the tree then it is in node.c[dir].
    // If found != null, either we just set it, or we set it on the path from the parent.  value is either in node, or node.c[dir] tree.

    if (!red(node) && !red(node.c[dir])) {
      // node.c[dir] != null && !red(node.c[dir]).  So node.c[dir] is a non-null black node.
      // Same for node: node is a non-null black node.
      
      if (red(node.c[!dir])) {
        // node is black, node.c[dir] is black, node.c[!dir] is red:
        //
        //           parent                         parent
        //             |                              |
        //          node, B                        !dir, B   <== new parent
        //        /         \        ==>          /       \
        //    !dir, R        \                   /      node, R
        //    /      \        \                 /       /      \
        //  a, B    b, B    dir, B            a, B    b, B    dir, B
        //
        // We can rotate @ node in the direction we need to descend, creating a 3-node in the mirror image.
        // !dir child is now at `node`'s height.
        parent.c[last] = rotate_single(node, dir);
        // grandparent = parent;  // We don't need this because we'll just reset it at the start of the next iteration.
        parent = parent.c[last];
        // `node` is red.
      }
      else {
        // node is non-null black.  node.c[dir] is non-null black.
        // node.c[!dir] is black. Is also not null, as that would be a BV.
        // node is an empty pseudonode.  This makes it more tricky to pass a red down.

        //if (!red(node.c[!dir])) {

        // parent != null.  Check sibling of `node`.
        Node * sibling = parent.c[!last];

        //           |                                     |
        //         parent                                parent
        //        ?      \                              /      ?
        //       ?        \                            /        ?
        //  sibling      node, B          or       node, B     sibling
        //              /       \                 /       \
        //             /         \               /         \
        //         !dir, B      dir, B       !dir, B      dir, B


        if (sibling != null) {
          // This branch should execute for all cases except the first iteration.
          // On the first iteration, `parent` is the superroot, so sibling is null.
          // On all others, sibling == null would be a BV.

          // Sibling is black.  WHY????
          // if (!sibling.black) writeln("Sibling not black!");

          // Parent color unknown.

          //               |
          //            parent, ?
          //          /          \
          //         /            \
          //   sibling, B         node, B         (or symmetrical as above)
          //   /      \          /       \
          //  /        \        /         \
          // ?          ?    !dir, B      dir, B

          if (!red(sibling.c[!last]) && !red(sibling.c[last])) {

            //               |
            //            parent, ?
            //          /          \
            //         /            \
            //   sibling, B         node, B         (or symmetrical)
            //   /      \          /       \
            //  /        \        /         \
            // B          B    !dir, B      dir, B

            // color flip
            parent.black = true;
            sibling.black = false;
            node.black = false;

            //               |
            //            parent, B
            //          /          \
            //         /            \
            //   sibling, R         node, R         (or symmetrical)
            //   /      \          /       \
            //  /        \        /         \
            // B          B    !dir, B      dir, B

            // Now `node` is red, so we can descend.
            // But if parent was black, we've just reduced its black height!?

          }
          else {
            // red(sibling.c[!last]) || red(sibling.c[last])
            // Thus sibling is black

            //               |
            //            parent, ?
            //          /          \
            //         /            \
            //   sibling, B         node, B         (or symmetrical)
            //   /      \          /       \
            //  /        \        /         \
            // ? (1+ red) ?    !dir, B      dir, B

            int dir2 = grandparent.c[1] == parent;  // which child is the parent?

            //          grandparent
            //                \
            //                 \
            //              parent, ?
            //            /          \
            //           /            \
            //     sibling, B         node, B         (or symmetrical for parent and grandparent directions)
            //     /      \          /       \
            //    /        \        /         \
            //   ? (1+ red) ?    !dir, B      dir, B

            if (red(sibling.c[last])) {

              //          grandparent
              //                \
              //                 \
              //              parent, ?
              //            /          \
              //           /            \
              //     sibling, B         node, B         (or symmetrical)
              //     /      \          /       \
              //    /        \        /         \
              //   ?   nephew,R    !dir, B      dir, B
              //      /        \
              //   foo, B     bar, B

              grandparent.c[dir2] = rotate_double(parent, last);

              //            grandparent
              //                  \
              //                   \
              //                nephew, B 
              //              /          \
              //             /            \
              //       sibling, B         parent, R
              //       /      \          /       \
              //      /        \        /         \
              //    ..       foo, B  bar, B     node, B
              //                               /       \
              //                              /         \
              //                          !dir, B      dir, B

              // Recolor.
              node.black = false;
              grandparent.c[dir2].black = false;
              grandparent.c[dir2].c[0].black = true;
              grandparent.c[dir2].c[1].black = true;

              //            grandparent
              //                  \
              //                   \
              //                nephew, R 
              //              /          \
              //             /            \
              //       sibling, B         parent, B
              //       /      \          /       \
              //      /        \        /         \
              //     ..      foo, B  bar, B     node, R
              //                               /       \
              //                              /         \
              //                          !dir, B      dir, B

            }
            else {
              // if (red(sibling.c[!last])) {      <== this test, in JSW, should not be necessary

              //            grandparent
              //                  \
              //                   \
              //                parent, ?
              //              /          \
              //             /            \
              //       sibling, B         node, B         (or symmetrical)
              //       /      \          /       \
              //      /        \        /         \
              //    nephew,R  foo, B  !dir, B      dir, B


              grandparent.c[dir2] = rotate_single(parent, last);

              //            grandparent
              //                  \
              //                   \
              //                sibling, B
              //              /          \
              //             /            \
              //       nephew, R         parent, R
              //                         /       \
              //                        /         \
              //                    foo, B      node, B
              //                               /       \
              //                              /         \
              //                          !dir, B      dir, B

              // Recolor.

              node.black = false;
              grandparent.c[dir2].black = false;
              grandparent.c[dir2].c[0].black = true;
              grandparent.c[dir2].c[1].black = true;

              //            grandparent
              //                  \
              //                   \
              //                sibling, R
              //              /          \
              //             /            \
              //       nephew, B         parent, B
              //                         /       \
              //                        /         \
              //                    foo, B      node, R
              //                               /       \
              //                              /         \
              //                          !dir, B      dir, B

              // }
              // else {
              //  writeln("Contradiction!");
              // }
            }
          }
        }
        else {
          // sibling == null.
          // This implies a BV.  We can only be here if `parent` is the superroot.
        }

        //}
        //else {
        //  writeln("Contradiction!");
        //}
      }
    }
    else {
      // (red(node) || red(node.c[dir])).  We also know node != null && node.c[dir] != null.
      // So either node is black, with node.c[dir] red child; or node is red, with node.c[dir] black child.
    }
  }

  if (found != null) {
    found.value = node.value;
    parent.c[parent.c[1] == node] = 
      node.c[node.c[0] == null];
    // delete node;
  }

  tree.root = superroot.c[1];
  if (tree.root != null) {
    tree.root.black = true;
  }

  // delete superroot;
}


struct Tree {
  Node * root;

  void print() {
    printNode(this.root);
  }

  bool search(int value) {
    return search_rec(this.root, value);
  }

  void insert(int value) {
    this.root = insert_rec(this.root, value);
    this.root.black = true;
  }

  void remove(int value) {
    remove_push_down_iter(&this, value);
  }

}


// Testing functions follow
// ------------------------

void printNode(Node * node, int depth = 0, string pre = "──") {

  void writeColor(string s, string mode) {

    void ansi(string m) {
      std.c.stdio.printf("%c", 27);
      writef("[%sm", m);
    }

    ansi(mode);
    write(s);
    ansi("0");
  }

  if (node != null) {
    printNode(node.c[1], depth+1, "╭╴");
    for(int i = depth; i > 0; i--) { write("  "); }
    write(pre);
    writeColor(std.conv.text(node.value), (node.black ? "0": "31"));
    writeln();
    printNode(node.c[0], depth+1, "╰╴");
  }
}


enum TreeType { Black, Red, Invalid }

struct TreeInfo {
  TreeType type;
  int height;

  string toString() {
    auto s = (type == TreeType.Black ? "Black" : type == TreeType.Red ? "Red" : "Invalid");
    if(type != TreeType.Invalid) s ~= ", " ~ std.conv.text(height);
    return s;
  }

  void print() { writeln(toString()); }
}

TreeInfo treeType(Node * node) {
  if (node == null) return TreeInfo(TreeType.Black, 0);
  if (!node.black && (red(node.c[0]) || red(node.c[1]))) return TreeInfo(TreeType.Invalid, 0);
  TreeInfo left = treeType(node.c[0]);
  TreeInfo right = treeType(node.c[1]);
  if(left.height != right.height) return TreeInfo(TreeType.Invalid, 0);
  if(node.black) return TreeInfo(TreeType.Black, left.height+1);
  else return TreeInfo(TreeType.Red, left.height);
}


bool test(int num_values, int num_tests, bool noisy) {

  auto testing = new Tree;
  auto reference = new bool[num_values];

  void printReference() {
    bool comma = false;
    write("{");
    for (int i = 0; i < num_values; i++) {
      if (reference[i]) {
        if (comma) write(", "); comma = true;
        writef("%d", i);
      }
    }
    writeln("}");
  }

  for(int i = 0; i < num_tests; i++) {
    auto value = std.random.uniform(0, num_values);

    if(std.random.uniform(0, 2)) {
      if (noisy) writef("Inserting %d...", value);
      reference[value] = true;
      testing.insert(value);
    }
    else {
      if (noisy) writef("Deleting %d...", value);
      reference[value] = false;
      testing.remove(value);
    }
    if (noisy) writeln("done.");

    if (noisy) {
      writeln("Reference:");
      printReference();
      writeln("Implementation:");
      testing.print();
    }

    auto info = treeType(testing.root);
    if (info.type != TreeType.Black) {
      info.print();
      writeln("Abort.");
      return false;
    }

    // Test search
    int test_search = std.random.uniform(0, num_values);

    if (noisy) {
      writef("Searching for %d...", test_search);
    }

    bool inReference =      reference[test_search];
    bool inImplementation = testing.search(test_search);

    if (inReference == inImplementation) {
      if (noisy) writeln("OK.");
    }
    else {
      writefln("discrepancy.  reference[%v] == %v.  implementation[%v] == %v.  Abort.", test_search, inReference, test_search, inImplementation);
      return false;
    }

    if (noisy) writeln("----");
  }

  testing.print();
  writeln("Tests passed.");
  return true;
}


void main() {
  test(1000, 100000, false);
}
