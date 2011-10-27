module bst.search.iterative;

import bst.node;
import bst.descend;


bool search(Node* root, in int value) {
  // Function precondition.
  /// Tree(root, S)

  bool found = false;

  // Assignment.
  // We're going to return `found`.
  // That is, the following loop establishes `found ↔ (value ∈ S)`.
  /// Tree(root, S) ∧ ¬found

  Node* i = root;

  // Assignment.
  /// Tree(root, S) ∧ ¬found ∧
  /// Tree(i, S)

  // a ↔ a
  /// Tree(root, S) ∧ ¬found ∧
  /// Tree(i, S) ∧ (value ∈ S) ↔ (value ∈ S)

  // <m:existsIntro/>
  /// Tree(root, S) ∧ ¬found ∧
  /// ∃Q. Tree(i, Q) ∧ (value ∈ Q) ↔ (value ∈ S)

  // false → anything (rule?).
  // This is our loop invariant.
  /// Tree(root, S) ∧
  /// ∃Q. Tree(i, Q) ∧ (value ∈ Q) ↔ (value ∈ S)
  /// ∧ found → (value ∈ S)

  // The size of Q strictly decreases with each iteration,
  // and the size of Q >= 0, as the empty set is the smallest possible set.
  // This shows that the loop terminates.

  while (i && !found) {
    // Loop invariant, and assert while-condition.
    /// Tree(root, S) ∧
    /// ∃Q. Tree(i, Q) ∧ (value ∈ Q) ↔ (value ∈ S)
    /// ∧ found → (value ∈ S)
    /// ∧ root ∧ ¬found

    // Non-null pointer is non-empty tree (lemma)
    /// Tree(root, S) ∧
    /// ∃Q. NonEmptyTree(i, Q) ∧ (value ∈ Q) ↔ (value ∈ S)
    /// ∧ found → (value ∈ S)
    /// ∧ ¬found

    // Open NonEmptyTree
    /// Tree(root, S) ∧
    /// ∃Q, v. TopOfTree(i, v, Q) ∧ (value ∈ Q) ↔ (value ∈ S)
    /// ∧ found → (value ∈ S)
    /// ∧ ¬found

    if (value == i.value) {
      // Assert if-condition.  Equality.
      /// Tree(root, S) ∧
      /// ∃Q. TopOfTree(i, value, Q) ∧ (value ∈ Q) ↔ (value ∈ S)
      /// ∧ found → (value ∈ S)
      /// ∧ ¬found

      // Root value in set (lemma).
      /// Tree(root, S) ∧
      /// ∃Q.
      ///   TopOfTree(i, value, Q) ∧
      ///   (value ∈ Q) ↔ (value ∈ S) ∧
      ///   value ∈ Q
      /// ∧ found → (value ∈ S)
      /// ∧ ¬found

      // →.  Weaken TopOfTree.
      /// Tree(root, S) ∧
      /// ∃Q. Tree(i, Q) ∧ (value ∈ Q) ↔ (value ∈ S)
      /// value ∈ S ∧ ¬found

      found = true;

      // Assignment.
      /// Tree(root, S) ∧
      /// ∃Q. Tree(i, Q) ∧ (value ∈ Q) ↔ (value ∈ S)
      /// value ∈ S ∧ found

      // →.  Re-establish invariant.
      /// Tree(root, S) ∧
      /// ∃Q. Tree(i, Q) ∧ (value ∈ Q) ↔ (value ∈ S)
      /// ∧ found → (value ∈ S)
    }
    else {
      // Assert else-condition.  Equality.
      /// Tree(root, S) ∧
      /// ∃Q, v. TopOfTree(i, v, Q) ∧ (value ∈ Q) ↔ (value ∈ S) ∧ v ≠ value
      /// ∧ found → (value ∈ S)
      /// ∧ ¬found

      Node* next = descend(i, value);

      // Assert else-condition.  Equality.
      /// Tree(root, S) ∧
      /// ∃Q, v. TopOfTree(i, v, Q) ∧ (value ∈ Q) ↔ (value ∈ S) ∧ v ≠ value
      /// ∧ found → (value ∈ S)
      /// ∧ ¬found

      i = next;
    }
  }
  return found;
}

