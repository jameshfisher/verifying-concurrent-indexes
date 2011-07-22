module bst.validate;

import bst.node;
import bst.index;


bool validate(bst.node.Node* n, int min, int max) {
  if (!n) return true;
  else {
    if (n.value < min || n.value > max) {
      return false;
    }
    else {
      return validate(n.c[0], min, n.value-1) && validate(n.c[1], n.value+1, max);
    }
  }
}


bool validate(bst.index.BstIndex i) {
  return validate(i.root, int.min, int.max);
}
