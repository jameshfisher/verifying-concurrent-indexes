module rb.blacken;

import rb.node;

Node* blacken(Node* root) {
  /// T(root, S) ∨ ∃ dir. RVT(root, S, dir)

  if (root != null && !root.black) {
    /// (T(root, S) ∧ root != null ∧ ¬root.black ∨
    ///  ∃ dir. RVT(root, S, dir) ∧ root != null ∧ ¬root.black) 

    /// 

    /// RT(root, S)
    root.black = true;
    /// BT(root, S)
  }
  else {
    //// T(root, S) &&
    //// (root == null || root.black)

    //// BT(root, S)
  }
  //// BT(root, S)

  return root;
}