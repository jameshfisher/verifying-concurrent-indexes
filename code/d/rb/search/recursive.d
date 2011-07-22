module rb.search.recursive;

import rb.node;

bool search(Node* root, int value) {
  if (root == null) return false;
  if (root.value == value) return true;
  return search(root.c[root.value < value], value);
}