module bst.search.recursive;

import bst.node;
import bst.descend;


bool search(Node* root, in int value) {
  // Function precondition.
  /// Tree(root, S)
  bool o;
  if (root) {
    // Assert if-test.
    /// Tree(root, S) ∧ root≠null

    // Non-null pointer is non-empty tree (lemma).
    /// NonEmptyTree(root, S)

    if (root.value == value) {
      // Assert if-test.  (Need lemma here.)
      /// TopOfTree(root, value, S)

      // Root value in set (lemma).
      /// TopOfTree(root, value, S) ∧ value ∈ S

      // Weaken TopOfTree.
      /// Tree(root, S) ∧ value ∈ S

      o = true;

      // Assignment.
      /// Tree(root, S) ∧ value ∈ S ∧ o = true

      // ???
      /// Tree(root, S) ∧ o ↔ (value ∈ S)
    }
    else {
      // Deny if-test.
      /// TopOfTree(root, v, S) ∧ v ≠ value

      Node* next = descend(root, value);

      // Specification of descend.
      /// TopOfTree(root, v, S) ∧
      /// ∃Q. Tree(o, Q) ∧ (value∈Q ↔ value∈S)) ∧ |Q| < |S|

      // Note recursion terminates because |Q| < |S| and |Q| >= 0
      o = search(next, value);

      // Specification of search.
      /// TopOfTree(root, v, S) ∧
      /// ∃Q. Tree(o, Q) ∧ (value∈Q ↔ value∈S)) ∧ |Q| < |S| ∧ o ↔ (value ∈ Q)

      // Transitivity of ↔; escape ∃; weaken TopOfTree.
      /// Tree(root, S) ∧ o ↔ (value ∈ S)
    }
    // Transitivity of both if-branches.
    /// Tree(root, S) ∧ o ↔ (value ∈ S)
  }
  else {
    // Deny if-test.
    /// Tree(root, S) ∧ root=null

    // Null pointer is empty tree (lemma).
    /// EmptyTree(root, S)

    // Empty tree is empty set (lemma).
    /// EmptyTree(root, S) ∧ S = ∅

    // Element not in empty set.
    /// EmptyTree(root, S) ∧ value ∉ S

    o = false;

    // Assignment.
    /// EmptyTree(root, S) ∧ value ∉ S ∧ o = false

    // ???
    /// EmptyTree(root, S) ∧ o ↔ (value ∈ S)

    // Weaken EmptyTree.
    /// Tree(root, S) ∧ o ↔ (value ∈ S)
  }
  // Function postcondition.
  // Postcondition of both if-branches.
  /// Tree(root, S) ∧ o ↔ (value ∈ S)
  return o;
}
