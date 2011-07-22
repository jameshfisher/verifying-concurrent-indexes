module bst.descend;

import bst.node;


Node* descend(Node* root, in int value) {
  // Function precondition.
  // The tree is non-empty and value is not at the root
  // (so if it's in the set, it's in one of the two subtrees).
  /// TopOfTree(root, v, S) ∧ v≠value

  int dir = value > root.value;

  // Either > or <.  C semantics render this as an integer.
  // v≠value is now implied.
  /// TopOfTree(root, v, S) ∧ 
  ///   (value < v ∧ dir = 0) ∨
  ///   (value > v ∧ dir = 1)

  Node* o = root.c[dir];

  // Open TopOfTree.  Do both cases of value of `dir`.
  /// ∃v,l,r,L,R.
  ///   root↦v,l,r ∗ Tree(l, L) ∗ Tree(r, R)
  ///   ∧ Compose(L, v, R, S)
  ///   ∧ (value < v ∧ o = l) ∨ (value > v ∧ o = r)

  // What is true of the subtree pointers is true of `o`.
  /// ∃v,l,r,L,R.
  ///   root↦v,l,r ∗ Tree(l, L) ∗ Tree(r, R)
  ///   ∧ Compose(L, v, R, S)
  ///   ∧ (value < v ∧ Tree(o, L)) ∨ (value > v ∧ Tree(o, R))

  // From Compose and relation of value and v.
  /// ∃v,l,r,L,R.
  ///   root↦v,l,r ∗ Tree(l, L) ∗ Tree(r, R)
  ///   ∧ Compose(L, v, R, S)
  ///   ∧ (Tree(o, L) ∧ (value∈L ↔ value∈S)) ∨ (Tree(o, R) ∧ (value∈R ↔ value∈S))

  // Introduce quantification.
  /// ∃v,l,r,L,R.
  ///   root↦v,l,r ∗ Tree(l, L) ∗ Tree(r, R)
  ///   ∧ Compose(L, v, R, S)
  ///   ∧ (∃Q. Tree(o, Q) ∧ (value∈Q ↔ value∈S)) ∨ (∃Q. Tree(o, Q) ∧ (value∈Q ↔ value∈S))

  // a ∨ a implies a.
  /// ∃v,l,r,L,R.
  ///   root↦v,l,r ∗ Tree(l, L) ∗ Tree(r, R)
  ///   ∧ Compose(L, v, R, S)
  ///   ∧ ∃Q. Tree(o, Q) ∧ (value∈Q ↔ value∈S))

  // Function postcondition.  Close TopOfTree.
  /// TopOfTree(root, v, S)
  /// ∧ ∃Q. Tree(o, Q) ∧ (value∈Q ↔ value∈S))

  // Note: we want |Q| < |S|
  // to show termination of other functions.

  return o;
}
