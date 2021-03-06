module bst.insert.iterative;

import bst.node;


const bool insert_parent_pointer = true;


static if (insert_parent_pointer) {

  Node* insert(Node * root, int value) {

    Node*  p = null,
      i = root;
    int dir;

    while (i) {
      if (value == i.value) return root;
      else {
	p = i;
	dir = value > i.value;
	i = i.c[dir];
      }
    }
    assert(!i);

    auto n = new Node(value);

    if (p) {
      p.c[dir] = n;
      return root;
    }
    else {
      return n;
    }
  }

} else {  // !insert_parent_pointer

  Node* insert(Node * root, int value) {

    if(!root) {
      root = new Node(value);
    }
    else {
      Node* i = root;
      while (i) {
	if (value == i.value) {
	  return root;
	}
	else {
	  int dir = value > i.value;
	  if (i.c[dir]) i = i.c[dir];
	  else {
	    i.c[dir] = new Node(value);
	    return root;
	  }
	}
      }
    }
    return root;
  }

}
