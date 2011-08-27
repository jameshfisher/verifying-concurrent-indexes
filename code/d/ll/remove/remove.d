module ll.remove.remove;

import ll.node;

static import ll.remove.recursive;
static import ll.remove.iterative;

const bool recursive = false;

Node* remove(Node* list, int value) {
  static if (recursive) {
    return ll.remove.recursive.remove(list, value);
  }
  else {
    return ll.remove.iterative.remove(list, value);
  }
}