#include "./Nodes.h"
#include "./Insert.h"

Np new(int x) {
  Np n = malloc(sizeof(*n));
  n->type = R;
  n->value = x;
  n->l = n->r = NULL;
  return n;
}


Np insert(Np root, int x) {
  Np o;

  if (root == NULL) {
    o = new(x);
  }
  else {
    // root != NULL
    if (x == root->value) {
      o = root;
    }
    else {
      // x != root->value
      if (x < root->value) {
	// x < root->value
	Np l_ = insert(root->left, x);
	
      }
      else {
	// x > root->value
      }
    }
  }

  return o;
}
