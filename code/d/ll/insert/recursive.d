module ll.insert.recursive;

import ll.node;

Node* insert(Node* list, int value) {
  Node* o;

  if (list) {
    if (list.value == value) {
      o = list;
    }
    else {
      if (list.value < value) {
	list.tail = insert(list.tail, value);
	o = list;
      }
      else {
	o = new Node(value, list);
      }
    }
  }
  else {
    o = new Node(value);
  }

  return o;
}