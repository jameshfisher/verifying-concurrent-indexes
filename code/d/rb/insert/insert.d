module rb.insert.insert;

import rb.node;

static import rb.insert.recursive;
static import rb.insert.iterative;

const bool recursive = false;

Node* insert(Node* root, int value) {
  static if (recursive) {
    return rb.insert.recursive.insert(root, value);
  }
  else {
    return rb.insert.iterative.insert(root, value);
  }
}