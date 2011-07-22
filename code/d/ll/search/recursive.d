module ll.search.recursive;

import ll.node;

bool search(Node* list, int value) {
  bool o;
  if (!list) {
    o = false;
  }
  else {
    if (list.value == value) {
      o = true;
    }
    else {
      if (list.value < value) {
	o = search(list.next, value);
      }
      else {
	o = false;
      }
    }
  }
  return o;
}