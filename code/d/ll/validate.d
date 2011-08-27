module ll.validate;

import ll.node;
import ll.index;


bool validate(ll.node.Node* n, int min) {
  if (!n) return true;
  if (n.value < min) return false;
  return validate(n.tail, n.value+1);
}


bool validate(ll.index.LlIndex i) {
  return validate(i.root, int.min);
}
