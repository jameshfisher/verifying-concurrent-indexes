module ll.search.search;

import ll.node;

static import ll.search.recursive;
static import ll.search.iterative;

const bool recursive = false;

bool search(Node* list, int value) {
  static if (recursive) {
    return ll.search.recursive.search(list, value);
  }
  else {
    return ll.search.iterative.search(list, value);
  }
}