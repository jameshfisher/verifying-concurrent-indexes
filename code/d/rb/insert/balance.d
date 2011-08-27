module rb.insert.balance;

import rb.node;
import rb.blacken;
static import rb.rotate;

Node* balance(Node* root, int dir) {
  if (red(root.c[dir])) {
    if (red(root.c[!dir])) {
      root.c[!dir] = blacken(root.c[!dir]);
      root.c[dir]  = blacken(root.c[dir]);
      root.black = false;
    }
    else {
      if (red(root.c[dir].c[dir])) {
	root = rb.rotate.single(root, !dir);
      }
      else {
	if (red(root.c[dir].c[!dir])) {
	  root = rb.rotate.dbl(root, !dir);
	}
      }
    }
  }
  return root;
}