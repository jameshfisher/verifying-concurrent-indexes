module bst.remove.removeMax.recursive;


import bst.node;
import bst.remove.removeMax.RemoveMaxRet;

import std.stdio;

RemoveMaxRet removeMax(Node* root) {
  // We can only remove the maximum from a non-empty tree!
  /// NonEmptyTree(root, S)

  assert(root != null);

  // Expand NonEmptyTree(root).
  /// ∃v. TopOfTree(root, v, S).

  // Expand TopOfTree(root, v, S).
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



  // We're going to put the return values here.
  int max;
  Node* newRoot;

  if (!r) {
    // Assert if-condition.  Discard EmptyTree.
    /// ∃v,l,L.
    ///   root↦v,l,null
    ///   ∗ Tree(l, L)
    ///   ∧ Compose(L, v, ∅, S).

    // Found-max (lemma on Compose).  Discard Compose.
    /// ∃v,l,L.
    ///   root↦v,l,null
    ///   ∗ Tree(l, L)
    ///   ∧ v∈S ∧ ∀x∈L. x &lt; v ∧ L = S - {v}.

    max = root.value;
    newRoot = root.c[0];

    // Assignment (twice).
    /// ∃L.
    ///   root↦max,newRoot,null
    ///   ∗ Tree(newRoot, L)
    ///   ∧ max∈S ∧ ∀x∈L. x &lt; max ∧ L = S - {max}.

    delete root;

    // `root` is dangling.  Rename bound variable.
    // This gives our function postcondition.
    /// ∃T.
    ///   Tree(newRoot, T)
    ///   ∧ max∈S ∧ ∀x∈T. x &lt; max ∧ T = S - {max}.
  }
  else {
    // Negate if-condition.
    /// ∃v,l,L,R.
    ///   root↦v,l,r
    ///   ∗ Tree(l, L)
    ///   ∗ Tree(r, R)
    ///   ∧ Compose(L, v, R, S)
    ///   ∧ r.

    // Non-null pointer is non-empty tree (lemma).
    /// ∃v,l,L,R.
    ///   root↦v,l,r
    ///   ∗ Tree(l, L)
    ///   ∗ NonEmptyTree(r, R)
    ///   ∧ Compose(L, v, R, S).

    auto d = removeMax(r);
    auto rightMax = d.max;
    auto rightRoot = d.root;

    // removeMax consumes NonEmptyTree(r, R).  `r` is now dangling.
    /// ∃v,l,L,R,N.
    ///   root↦v,l,r
    ///   ∗ Tree(l, L)
    ///   ∗ Tree(rightRoot, N)
    ///   ∧ Compose(L, v, R, S)
    ///   ∧ rightMax∈R ∧ ∀x∈N. x &lt; rightMax ∧ N = R - {rightMax}.

    root.c[1] = rightRoot;

    // Assignment.
    /// ∃v,l,L,R,N.
    ///   root↦v,l,rightRoot
    ///   ∗ Tree(l, L)
    ///   ∗ Tree(rightRoot, N)
    ///   ∧ Compose(L, v, R, S)
    ///   ∧ rightMax∈R ∧ ∀x∈N. x &lt; rightMax ∧ N = R - {rightMax}.

    // Close TopOfTree.
    /// ∃v.
    ///   TopOfTree(root, v, S - {rightMax})
    ///   ∧ rightMax∈S ∧ ∀x∈(S-{rightMax}). x &lt; rightMax.

    // Introduce quantification on S - {rightMax}.
    /// ∃v, T.
    ///   TopOfTree(root, v, T)
    ///   ∧ rightMax∈S ∧ ∀x∈T. x &lt; rightMax ∧ T = S - {rightMax}.

    // Close NonEmptyTree.
    /// ∃T.
    ///   NonEmptyTree(root, T)
    ///   ∧ rightMax∈S ∧ ∀x∈T. x &lt; rightMax ∧ T = S - {rightMax}.    

    max = rightMax;
    newRoot = root;

    // Assignment.  Gives postcondition.
    /// ∃T.
    ///   Tree(newRoot, T)
    ///   ∧ max∈S ∧ ∀x∈T. x &lt; max ∧ T = S - {max}.
  }

  // Postcondition of both if branches.  
  // Function postcondition.
  // `max` is the maximum element in `S`,
  // and `newRoot` represents `S` with `max` subtracted.
  /// ∃T.
  ///   Tree(newRoot, T)
  ///   ∧ max∈S ∧ ∀x∈T. x &lt; max ∧ T = S - {max}.


  RemoveMaxRet o = {max: max, root: newRoot};
  return o;
}