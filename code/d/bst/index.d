module bst.index;

import index;

static import bst.node;
static import bst.search.search;
static import bst.insert.insert;
static import bst.remove.remove;
static import bst.contents;


class BstIndex : index.Index {
  bst.node.Node * root;

  bool search(int value) {
    return bst.search.search.search(root, value);
  }

  void insert(int value) {
    root = bst.insert.insert.insert(root, value);
  }

  void remove(int value) {
    root = bst.remove.remove.remove(root, value);
  }

  int[] contents() {
    return bst.contents.contents(root);
  }
}
