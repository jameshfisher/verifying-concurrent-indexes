module ll.insert.insert;

import ll.node;

static import ll.insert.recursive;
static import ll.insert.iterative;

const bool recursive = false;

Node* insert(Node* list, int value) {
  static if (recursive) {
    return ll.insert.recursive.insert(list, value);
  }
  else {
    return ll.insert.iterative.insert(list, value);
  }
}