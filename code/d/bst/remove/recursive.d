module bst.remove.recursive;

import bst.node;
import bst.remove.removeRoot;


Node* remove(Node* root, int value) {
  Node* o;
  
  if (!root) {
    o = null;
  }
  else {
    if (value == root.value) {
      o = removeRoot(root);
    }
    else {
      o = root;
      int dir = value > root.value;
      o.c[dir] = remove(o.c[dir], value);
    }
  }

  return o;
}