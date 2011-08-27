module rb.remove.recursive;

import rb.node;
import rb.blacken;
static import rb.rotate;
import rb.print;

import std.stdio;


int findMax(Node* root) {
  //// T(root, S)

  int max;

  if (root.c[1]) {
    //// R != empty
    max = findMax(root.c[1]);
    //// max is max(R)
    //// max(R) is max(S)
  }
  else {
    //// R is empty
    max = root.value;
    //// max is max({value})
    //// max is max(S)
  }
  //// max is max(S)

  return max;
}

Node* clr(bool c, Node* root, bool* fixed) {
  //// root some color * T(root.c[0], L, h) * T(root.c[0], R, i) && !fixed

  *fixed = true;
  root.black = c;
  root.c[0].black = root.c[1].black = true;

  //// fixed && (c == red && RT(root, S)) || (c == black && EBT(root, S))

  return root;
}

Node* balB(Node* root, int dir, bool* fixed) {
  //// !fixed

  Node* o;

  if (red(root.c[!dir].c[!dir])) {
    //// T(fix, h) * root     * T(near, h) * black sibling * RT(far, h)
    bool oldColor = root.black;
    root = rb.rotate.single(root, dir);
    //// T(fix, h) * red root * T(near, h) * black sibling * RT(far, h)
    o = clr(oldColor, root, fixed);
    //// fixed && 
  }
  else {
    if (red(root.c[!dir].c[dir] )) {
      o = clr(root.black, rb.rotate.dbl(   root, dir), fixed);
    }
    else {
      *fixed = !root.black;
      root.black = true;
      root.c[!dir].black = false;
      o = root;
    }
  }

  return o;
}


Node* balance(Node* root, int dir, bool* fixed) {
  // one less BH in `dir`. root.c[!dir] != null.

  if (red(root.c[!dir])) {  // do case-reducing rotation.
    root = rb.rotate.single(root, dir);
    root.c[dir] = balB(root.c[dir], dir, fixed);
    return root;
  }
  else return balB(root, dir, fixed);
}


Node* try_blacken(Node* root, bool b, bool* fixed) {
  if (!b) {
    *fixed = true;
    return root;
  }
  else if (red(root)) {
    *fixed = true;
    return blacken(root);
  }
  else return root;
}


Node* remove_aux(Node* root, int value, bool* fixed) {
  if (root == null) {
    *fixed = true;
    return root;
  }
  else if (root.value == value) {
    if (root.c[0]) {
      if (root.c[1]) {
	int max = findMax(root.c[0]);
	root.value = max;
	root.c[0] = remove_aux(root.c[0], max, fixed);
	return (*fixed) ? root : balance(root, 0, fixed);
      }
      else return try_blacken(root.c[0], root.black, fixed);
    }
    else return try_blacken(root.c[1], root.black, fixed);
  }
  else {
    int dir = root.value < value;
    root.c[dir] = remove_aux(root.c[dir], value, fixed);
    return (*fixed) ? root : balance(root, dir, fixed);
  }
}


Node* remove(Node* root, int value) {
  bool* dummy = new bool;
  *dummy = false;
  return blacken(remove_aux(root, value, dummy));
}