module rb.insert.recursive;

import rb.node;
import rb.blacken;
import rb.insert.balance;

Node* insert_aux(Node* root, int w) {
  if (root == null) return new Node(w);
  else if (root.value == w) return root;
  int dir = root.value < w;
  root.c[dir] = insert_aux(root.c[dir], w);
  return balance(root, dir);
}


Node* insert(Node* root, int value) {
  return blacken(insert_aux(root, value));
}