module ll.insert.iterative;

import ll.node;

Node* insert(Node* list, int value) {
  Node* o;

  if (!list || value < list.value) {
    o = new Node(value, list);
  }
  else {
    if (value == list.value) {
      o = list;
    }
    else {
      Node* p = list;
      while (p.tail && p.tail.value < value) {
	p = p.tail;
      }
      // !p.tail || p.tail.value >= value
      if (!p.tail || p.tail.value > value) {
	p.tail = new Node(value, p.tail);
      }
      o = list;
    }
  }

  return o;
}