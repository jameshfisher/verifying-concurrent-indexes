module rb.blacken;

import rb.node;

Node* blacken(Node* root) {
  /// T(root, S) ∨ ∃ dir. RVT(root, S, dir)

  if (root == null) {
    // skip
  }
  else {
    if (root.black) {
      // skip
    }
    else {
      root.black = true;
    }
  }

  return root;
}