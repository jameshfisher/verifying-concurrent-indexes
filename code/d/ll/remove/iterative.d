module ll.remove.iterative;

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
      Node* i = n;
      while (i.tail && i.tail.value < value) {
	i = i.tail;
      }
      // !i.tail || value >= i.tail.value

      if (i.tail && i.tail.value == value) {
	Node* d = i.tail;
	i.tail = i.tail.tail;
	delete d;
      }

      o = n;
    }
  }

  return o;
}