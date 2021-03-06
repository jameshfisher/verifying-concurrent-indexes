module ll.index;

import index;

static import ll.node;
static import ll.search.search;
static import ll.insert.insert;
static import ll.remove.remove;
static import ll.contents;


class LlIndex : index.Index {
  ll.node.Node * root;

  bool search(int value) {
    return ll.search.search.search(root, value);
  }

  void insert(int value) {
    root = ll.insert.insert.insert(root, value);
  }

  void remove(int value) {
    root = ll.remove.remove.remove(root, value);
  }

  int[] contents() {
    return ll.contents.contents(root);
  }
}
