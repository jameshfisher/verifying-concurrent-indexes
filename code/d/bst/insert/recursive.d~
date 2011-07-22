module bst.insert.recursive;

import bst.node;

Node* insert(Node* root, int value) {
  Node* o;
  if (!root) {
    o = new Node(value);
  }
  else {
    if (root.value == value) {
      o = root;
    }
    else {
      if (value < root.value) {
	root.c[0] = insert(root.c[0], value);
      }
      else {
	root.c[1] = insert(root.c[1], value);
      }
      o = root;
    }
  }
  return o;
}