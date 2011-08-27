module rb.remove.remove;

import rb.node;

static import rb.remove.recursive;
static import rb.remove.iterative;

const bool recursive = true;

Node* remove(Node* root, int value) {
  static if (recursive) {
    return rb.remove.recursive.remove(root, value);
  }
  else {
    return rb.remove.iterative.remove(root, value);
  }
}