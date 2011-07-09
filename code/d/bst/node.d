module bst.node;

struct Node {
  int value;
  Node*[2] c;

  this(int value) {
    this.value = value;
  }
}

// A predicate on the sets of subtrees and the value at the root.
/// Compose(L, v, R, S) ≝
///   L ∪ {v} ∪ R = S
///   ∧ ∀l∈L. l < v
///   ∧ ∀r∈R. v < r.

// Less-than in left (lemma).
/// Compose(L, v, R, S) ∧ w<v
///   → (w∈L ↔ w∈S).

// Greater-than in right (lemma).
/// Compose(L, v, R, S) ∧ v<w
///   → (w∈R ↔ w∈S).

// An empty tree is easy:
/// EmptyTree(n, S) ≝
///   emp
///   ∧ n = null
///   ∧ S = ∅.

// It's convenient to talk about the value at the top of the tree.
/// TopOfTree(n, v, S) ≝
///   ∃l,r,L,R.
///     n↦v,l,r
///     ∗ Tree(l, L)
///     ∗ Tree(r, R)
///     ∧ Compose(L, v, R, S).

// A non-empty tree is a TopOfTree with unknown value at the root.
/// NonEmptyTree(n, S) ≝
///   ∃v. TopOfTree(n, v, S).

// A tree is simply either empty or not empty.
/// Tree(n, S) ≝
///   EmptyTree(n, S) ∨ 
///   NonEmptyTree(n, S).
