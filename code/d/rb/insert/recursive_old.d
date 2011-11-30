module rb.insert.recursive;

import rb.node;
import rb.rotate;

import std.stdio;

Node* insert_aux(Node* root, int value) {

  Node* o;
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

    Node** into = &root.c[dir];

    bool red_before = red(*into);
    *into = insert_aux(*into, value); // Do the insert.
    bool red_after = red(*into);

    if (red_before && !red_after) {
      writeln("Red -> Black!");
    }

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
        Node** far_child = &(*into).c[dir];
        if (red(*far_child)) {
          // into is red, far child is red.  RV.  sibling is black.

          //         root, R/B  &lt;== was previously either BT(N) or RT(N-1)
          //       /           \
          //      /             \
          // sibling, B(N-1)   into, RVT(dir, N-1)
          //                    /       \
          //                   /         \
          //     near child, B(TN-1)    far child, R(N-1)

          assert(!red(root));   // WHY???

          rb.rotate.single_ptr(&root, !dir);

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
          Node** near_child = &(*into).c[!dir];
          if (red(*near_child)) {

            assert(!red(root));   // WHY???

            // near child is red.

            //         root, R/B  &lt;== was previously either BT(N) or RT(N-1)
            //       /           \
            //      /             \
            // sibling, B(N-1)   into, RVT(!dir, N-1)
            //                    /           \
            //                   /             \
            //        near_child, RT(N-1)     far child, BT(N-1)
            //          /             \
            //         /               \
            //  near_gc, BT(N-1)  far_gc, BT(N-1)

            rb.rotate.dbl_ptr(&root, !dir);

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


Node* insert(Node* root, int value) {
  Node* r = insert_aux(root, value);
  r.black = true;
  return r;
}