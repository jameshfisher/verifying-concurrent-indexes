module rb.search.search;

import rb.node;

static import rb.search.recursive;

bool search(Node * root, int value) {
  return rb.search.recursive.search(root, value);
}