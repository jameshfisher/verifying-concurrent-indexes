module rb.contents;

import rb.node;

int[] contents(Node* node) {
  if (!node) return [];
  else return contents(node.c[0]) ~ [node.value] ~ contents(node.c[1]);
}
