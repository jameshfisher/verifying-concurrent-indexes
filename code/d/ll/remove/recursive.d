module ll.remove.recursive;

import ll.node;

Node* remove(Node* n, int value) {
  Node* o;

  if (!n) {
    o = null;
  }
  else {
    if (n.value == value) {
      o = n.tail;
      delete n;
    }
    else {
      if (n.value < value) {
	n.tail = remove(n.tail, value);
	o = n;
      }
      else {
	o = n;
      }
    }
  }

  return o;
}