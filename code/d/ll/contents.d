module ll.contents;

import ll.node;

int[] contents(Node* root) {
  if (root == null) return [];
  return [root.value] ~ contents(root.tail);
}
