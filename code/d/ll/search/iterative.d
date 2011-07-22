module ll.search.iterative;

import ll.node;

bool search(Node* list, int value) {

  while (list && list.value < value) {
    list = list.next;
  }
  // (!list || list.value >= value)

  bool o;
  if (!list) {
    o = false;
  }
  else {
    o = list.value == value;
  }

  return o;
}