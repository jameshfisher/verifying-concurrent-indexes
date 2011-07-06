module rb.search;

import rb.node;

bool search_rec(Node * root, int value) {
  if (root == null) return false;
  if (root.value == value) return true;
  return search_rec(root.c[root.value < value], value);
}

