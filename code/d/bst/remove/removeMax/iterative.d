module bst.remove.removeMax.iterative;

import bst.node;
import bst.remove.removeMax.RemoveMaxRet;

import std.stdio;

RemoveMaxRet removeMax(Node* root) {
  // We can only remove the maximum from a non-empty tree!
  /// NonEmptyTree(root, S)

  assert(root != null);

  // We're going to put the return values here.
  int max;
  Node* newRoot;

  // Open NonEmptyTree(root, S).
  /// ∃v. TopOfTree(root, v, S).

  // Open TopOfTree.
  /// ∃v,l,r,L,R.
  ///   root↦v,l,r
  ///   ∗ Tree(l, L)
  ///   ∗ Tree(r, R)
  ///   ∧ Compose(L, v, R, S).

  auto r = root.c[1];

  // Take `r` out of quantification.
  /// ∃v,l,L,R.
  ///   root↦v,l,r
  ///   ∗ Tree(l, L)
  ///   ∗ Tree(r, R)
  ///   ∧ Compose(L, v, R, S).

  if(!r) {
    // Assert if-condition.  Discard EmptyTree.  The right child is null, so the maximum is at the root.  The rest of the set is in the LHS.
    /// ∃v,l,L.
    ///   root↦v,l,null
    ///   ∗ Tree(l, L)
    ///   ∧ Compose(L, v, ∅, S).

    // Found-max (lemma on Compose).  Discard Compose.
    /// ∃v,l,L.
    ///   root↦v,l,null
    ///   ∗ Tree(l, L)
    ///   ∧ v∈S ∧ ∀x∈L. x < v ∧ L = S - {v}.

    max = root.value;
    newRoot = root.c[0];

    // Assignment.
    /// ∃L.
    ///   root↦max,newRoot,null
    ///   ∗ Tree(newRoot, L)
    ///   ∧ max∈S ∧ ∀x∈L. x < max ∧ L = S - {max}.

    delete root;

    // Deletion.  Rename variable.  This is our postcondition.
    /// ∃T.
    ///   Tree(newRoot, T)
    ///   ∧ max∈S ∧ ∀x∈T. x < max ∧ T = S - {max}.
  }
  else {
    // We maintain a run of two nodes in the tree: current node i and its parent p.
    // This run moves down the right spine until i.c[1] is null.
    // At this point, i is the right-most node, thus containing the maximum element.
    // We then remove i using the usual splice-out method (i has at most one child).
    
    Node* p = root;
    Node* i = r;

    /// Tree(root, S).  Descendant(root, p).  Child(p, i).
    /// Descendant -> subset.
    /// Child -> subset.
    /// i contains non-empty set I, which is a subset of S.
    /// maximum value of S is in I.

    while (i.c[1]) {
      p = i;
      i = i.c[1];
    }
    //

    assert(!i.c[1]);
    assert(p);
    assert(i);
    assert(p.c[1] == i);

    p.c[1] = i.c[0];

    max = i.value;

    delete i;

    newRoot = root;
  }

  /// ∃T.
  ///   Tree(newRoot, T)
  ///   ∧ max∈S ∧ ∀x∈T. x < max ∧ T = S - {max}.

  RemoveMaxRet o = {max: max, root: newRoot};
  return o;
}