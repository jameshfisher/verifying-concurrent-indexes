module bst.remove.iterative;

import bst.node;
import bst.remove.removeRoot;


Node * remove(Node * root, int value) {
  Node * o;
  if (!root) {
    o = null;
  }
  else if (value == root.value) {
    o = removeRoot(root);
  }
  else {
    o = root;

    Node* p = o;
    int dir = value > p.value;
    Node* i = p.c[dir];

    // iterate until i is the node to delete
    // then set link in p to removeRoot(i).
    while (i && i.value != value) {
      p = i;
      dir = value > p.value;
      i = i.c[dir];
    }
    assert(!i || i.value == value);
    assert(dir == (value > p.value));

    if (i) {
      p.c[dir] = removeRoot(i);
    }
  }

  return o;
}
