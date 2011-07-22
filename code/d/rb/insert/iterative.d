module rb.insert.iterative;

import rb.node;
import rb.rotate;

/* this is the top-down algorithm. */

Node* insert(Node* root, int value) {
  Node* o;

  if (root == null) {
    o = new Node(value);
  }
  else {

    Node* superroot = new Node(0); superroot.black = true;
    superroot.c[1] = root;
    int dir = 0, last;

    Node* greatgp = superroot,
          grandparent = null,
          parent = null,
          node = root;

    bool done = false;

    while (!done) {
      if (node == null) {
        parent.c[dir] = node = new Node(value);
      }

      if (red(node.c[0]) && red(node.c[1])) {
        node.black = false;
        node.c[0].black = true;
        node.c[1].black = true;
      }

      if (red(node) && red(parent)) {
        int dir2 = (greatgp.c[1] == grandparent);
        if (node == parent.c[last]) {
          greatgp.c[dir2] = rb.rotate.single(grandparent, !last);
        }
        else {
          greatgp.c[dir2] = rb.rotate.dbl(grandparent, !last);
        }
      }

      if (node.value == value) {
        done = true;
      }
      else {
        last = dir;
        dir = node.value < value;

        if (grandparent != null) greatgp = grandparent;
        grandparent = parent;
        parent = node;
        node = node.c[dir];
      }
    }

    o = superroot.c[1];
  }

  o.black = true;

  return o;
}
