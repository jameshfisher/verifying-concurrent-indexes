module bst.search;

import bst.node;

bool search(Node* root, int value) {
  while (root) {
    if (value == root.value) return true;
    else root = root.c[value > root.value];
  }
  assert(!root);
  return false;
}

unittest {
  Node* a = null;
  assert(!search(a, 1));
}

unittest {
  Node* a = new Node(5);
  assert(search(a, 5));
  assert(!search(a, 6));
}

unittest {
  Node* a = new Node(5);
  a.c[0] = new Node(4);
  assert(search(a, 4));
  assert(!search(a, 3));
  assert(!search(a, 6));
}
