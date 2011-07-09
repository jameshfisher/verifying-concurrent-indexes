module bst.search;

import bst.node;
static import bst.search_recursive;
static import bst.search_iterative;

const bool recursive = false;


bool search(Node* root, in int value) {
  static if (recursive) {
    return bst.search_recursive.search(root, value);
  } else {
    return bst.search_iterative.search(root, value);
  }
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
