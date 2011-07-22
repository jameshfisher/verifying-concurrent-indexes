module bst.removeRoot;

import bst.node;
import bst.remove.removeMax.removeMax;
import bst.remove.removeMax.RemoveMaxRet;


Node* removeRoot(Node* root) {
  assert(root != null);

  if (root.c[0] && root.c[1]) {
    RemoveMaxRet r = removeMax(root.c[0]);
    root.value = r.max;
    root.c[0] = r.root;
  }
  else {
    assert(!root.c[0] || !root.c[1]);
    Node* rem = root;
    if (root.c[0]) {
      root = root.c[0];
    }
    else {
      root = root.c[1];
    }
    // delete rem;
  }
  return root;
}


unittest {
  auto a = new Node(5);
  a = removeRoot(a);
  assert(a == null);
}


unittest {
  auto a = new Node(5);
  a.c[0] = new Node(4);
  a = removeRoot(a);
  assert(a);
  assert(a.value == 4);
  assert(!a.c[0]);
  assert(!a.c[1]);
}


unittest {
  auto a = new Node(5);
  a.c[0] = new Node(4);
  a.c[1] = new Node(6);
  a = removeRoot(a);

  assert(a);
  assert(a.value == 4);
  assert(a.c[0] == null);
  assert(a.c[1]);
  assert(a.c[1].value == 6);
  assert(!a.c[1].c[0]);
  assert(!a.c[1].c[1]);
}
