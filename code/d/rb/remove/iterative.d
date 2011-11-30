module rb.remove.iterative;

import rb.node;
import rb.rotate;

Node * remove(Node * root, int value) {
  if (root == null) return null;                // The easy case: deletion from {} is {}, with no balancing.

  auto superroot = new Node(0); superroot.black = true;   // A false tree root.  Simplifies cases.
  superroot.c[1] = root;  int dir = 1;  // The false root has the real root as its right child.

  Node * found = null;  // If we find the value on the way down, and it's not a leaf, we put the node here.
  Node * node = superroot;                      // `node` will descend the tree to a leaf.
  Node * parent = null, grandparent = null;     // The parent and grandparent of `node`.

  // We'll continue down the tree, after which `node` will be the dummy to remove the value from.
  // Then if `found` is null, we can just remove `node`.  Otherwise, we move `node`'s value into `found`, then remove node.

  // We ensure that before descending one level, we're not descending to an empty pseudo-node.

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
      // node.c[dir] != null &amp;&amp; !red(node.c[dir]).  So node.c[dir] is a non-null black node.
      // Same for node: node is a non-null black node.
      
      if (red(node.c[!dir])) {
        // node is black, node.c[dir] is black, node.c[!dir] is red:
        //
        //           parent                         parent
        //             |                              |
        //          node, B                        !dir, B   &lt;== new parent
        //        /         \        ==&gt;          /       \
        //    !dir, R        \                   /      node, R
        //    /      \        \                 /       /      \
        //  a, B    b, B    dir, B            a, B    b, B    dir, B
        //
        // We can rotate @ node in the direction we need to descend, creating a 3-node in the mirror image.
        // !dir child is now at `node`'s height.
        parent.c[last] = rb.rotate.single(node, dir);
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

              grandparent.c[dir2] = rb.rotate.dbl(parent, last);

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
              // if (red(sibling.c[!last])) {      &lt;== this test, in JSW, should not be necessary

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


              grandparent.c[dir2] = rb.rotate.single(parent, last);

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
      // (red(node) || red(node.c[dir])).  We also know node != null &amp;&amp; node.c[dir] != null.
      // So either node is black, with node.c[dir] red child; or node is red, with node.c[dir] black child.
    }
  }

  if (found != null) {
    found.value = node.value;
    parent.c[parent.c[1] == node] = 
      node.c[node.c[0] == null];
    // delete node;
  }

  root = superroot.c[1];
  if (root != null) {
    root.black = true;
  }

  // delete superroot;

  return root;
}
