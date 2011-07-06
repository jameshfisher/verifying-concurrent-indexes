module bst.contents;

import bst.node;

int[] contents(Node* root) {
  if (root == null) return [];
  return contents(root.c[0]) ~ [root.value] ~ contents(root.c[1]);
}
