module bst.insert.insert;

import bst.node;

static import bst.insert.iterative;
static import bst.insert.recursive;


const bool recursive = true;

Node * insert(Node* root, int value) {
  static if (recursive) {
    return bst.insert.recursive.insert(root, value);
  }
  else {
    return bst.insert.iterative.insert(root, value);
  }
}

unittest {
  Node* a = null;

  a = insert(a, 5);

  auto ass = (){
    assert(a);
    assert(a.value == 5);
    assert(!a.c[0]);
    assert(!a.c[1]);
  };
  ass();
  a = insert(a, 5);
  ass();

  a = insert(a, 6);
  ass = (){
    assert(a);
    assert(a.value == 5);
    assert(!a.c[0]);
    assert(a.c[1]);
    assert(a.c[1].value == 6);
    assert(!a.c[1].c[0]);
    assert(!a.c[1].c[1]);
  };
  ass();
  a = insert(a, 6);
  ass();
}
