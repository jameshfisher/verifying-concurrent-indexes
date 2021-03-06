module bst.remove.remove;

import bst.node;

static import bst.remove.recursive;
static import bst.remove.iterative;


const bool recursive = true;


Node * remove(Node * root, int value) {
  static if (recursive) {
    return bst.remove.recursive.remove(root, value);
  }
  else {
    return bst.remove.iterative.remove(root, value);
  }
}


unittest {
  assert(remove(null, 5) == null);
}

unittest {
  auto a = new Node(5);
  a = remove(a, 5);
  assert(a == null);
}

unittest {
  auto a = new Node(5);
  a = remove(a, 6);
  assert(a);
  assert(a.value == 5);
  assert(!a.c[0]);
  assert(!a.c[1]);
}

unittest {
  auto a = new Node(5);
  a.c[1] = new Node(7);
  a.c[1].c[0] = new Node(6);
  a = remove(a, 7);
  assert(a);
  assert(a.value == 5);
  assert(!a.c[0]);
  assert(a.c[1]);
  assert(a.c[1].value == 6);
  assert(!a.c[1].c[0]);
  assert(!a.c[1].c[1]);
}
