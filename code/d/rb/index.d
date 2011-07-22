module rb.index;

static import index;

static import rb.node;
static import rb.search.search;
static import rb.insert.insert;
static import rb.remove;
static import rb.contents;

import std.stdio;

class RbIndex : index.Index {
  rb.node.Node * root;

  bool search(int value) {
    return rb.search.search.search(this.root, value);
  }

  void insert(int value) {
    this.root = rb.insert.insert.insert(this.root, value);
  }

  void remove(int value) {
    this.root = rb.remove.remove_push_down_iter(this.root, value);
  }

  int[] contents() {
    return rb.contents.contents(root);
  }
}
