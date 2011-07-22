module bst.removeMax;

import bst.node;
import bst.remove.removeMax.RemoveMaxRet;
static import bst.remove.removeMax.iterative;
static import bst.remove.removeMax.recursive;

const bool recursive = false;

RemoveMaxRet removeMax(Node* root) {
  static if (recursive) {
    return bst.remove.removeMax.recursive.removeMax(root);
  }
  else {
    return bst.remove.removeMax.iterative.removeMax(root);
  }
}


unittest {
  // Removing max from single-element set yields single element and empty set.
  auto a = new Node(5);
  auto b = removeMax(a);
  assert(b.max == 5);
  assert(b.root == null);
}

unittest {
  // Removing max from two-element set with max at root yields root element and left set.
  auto c = new Node(5);
  c.c[0] = new Node(3);
  auto d = removeMax(c);
  assert(d.max == 5);
  assert(d.root != null);
  assert(d.root.value == 3);
  assert(d.root.c[0] == null);
  assert(d.root.c[1] == null);
}

unittest {
  // Removing max element with left child
  auto a = new Node(0);
  a.c[1] = new Node(4);
  a.c[1].c[0] = new Node(3);

  auto after = removeMax(a);
  assert(a);
  assert(a.value == 0);
  assert(!a.c[0]);
  assert(a.c[1]);
  assert(a.c[1].value == 3);
  assert(!a.c[1].c[0]);
  assert(!a.c[1].c[1]);
}
