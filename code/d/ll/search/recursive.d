module ll.search.recursive;

import ll.node;

bool search(Node* list, int v) {
  /// List(list, S)
  bool o;
  if (list == null) {
    // Assert if-condition.
    /// List(list, S) ∧ list = null

    // Null pointer is empty list.
    /// EmptyList(list, S)

    // v is not in the empty set.
    /// v ∉ S

    o = false;

    // Assignment.
    /// v ∉ S ∧ o = false
  }
  else {
    // Deny if-condition.
    /// List(list, S) ∧ list ≠ null

    // Non-null is non-empty.
    /// NonEmptyList(list, S)

    // Open NonEmptyList.
    /// ∃value, tail, T.
    /// list ↦ value, tail ∗
    /// List(tail, T) ∧
    /// Compose(value, T, S)

    if (list.value == value) {
      // Assert if-condition.  Frame off heap.
      /// ∃T. Compose(v, T, S)

      // Head of list in set.
      /// v ∈ S

      o = true;

      // Assignment.
      /// v ∈ S ∧ o = true

      /// o ↔ v ∈ S
    }
    else {
      // Deny if-condition.
      /// ∃value, tail, T.
      /// list ↦ value, tail ∗
      /// List(tail, T) ∧
      /// Compose(value, T, S)
      /// ∧ value ≠ v
      if (list.value < v) {
        // Assert if-condition.
        /// ∃value, tail, T.
        /// list ↦ value, tail ∗
        /// List(tail, T) ∧
        /// Compose(value, T, S)
        /// ∧ value < v
        o = search(list.tail, v);

        // Inductive use of specification.
        /// ∃value, tail, T.
        /// list ↦ value, tail ∗
        /// List(tail, T) ∧
        /// Compose(value, T, S)
        /// ∧ value < v
        /// ∧ o ↔ v ∈ T

        // In tail is in set.
        /// o ↔ v ∈ S
      }
      else {
        // Deny if-condition.  Frame off heap.
        /// ∃value, T. Compose(value, T, S)
        /// ∧ v < value

        // Less-than-head not in list.
        /// v ∉ S
        o = false;

        // Assignment.
        /// v ∉ S ∧ o = false

        /// o ↔ v ∈ S
      }
      /// o ↔ v ∈ S
    }
    /// o ↔ v ∈ S
  }

  /// o ↔ v ∈ S

  return o;
}