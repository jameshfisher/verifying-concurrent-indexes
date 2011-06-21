
//-----------------------------------------------------------------//
//                             rotate                              //
//-----------------------------------------------------------------//

/* Performs the following transformation:

     |                    |
    node                  L
   /    \                / \
  L      R             LL  node
 / \                       / \
LL  LR                    LR  R

*/


pLLRBNode llrb_rotate_right(pLLRBNode node) {
  /*
    ∃ value, left, right, L, R, lv, ll, lr, LL, LR.
          ( node↦value,left,right
          ∗ Tree(right, R)
          ∗ left↦lv,ll,lr
          ∗ Tree(ll, LL)
          ∗ Tree(lr, LR) )
       && LL ∪ {lv} ∪ LR ∪ {value} ∪ R == S
       && (∀ v∈LL. v < lv)
       && (∀ v∈LR. v > lv && v < value)
       && (∀ v∈R. v > value) )
  */
  pLLRBNode left = node->left;
  // left out of ∃
  pLLRBNode lr = left->right;
  // ... && lr out of ∃
  left->right = node;
  /*
    ∃ value, right, L, R, lv, ll, LL, LR.
          ( node↦value,left,right
          ∗ Tree(right, R)
          ∗ left↦lv,ll,node
          ∗ Tree(ll, LL)
          ∗ Tree(lr, LR) )
       && LL ∪ {lv} ∪ LR ∪ {value} ∪ R == S
       && (∀ v∈LL. v < lv)
       && (∀ v∈LR. v > lv && v < value)
       && (∀ v∈R. v > value) )
  */
  node->left = lr;
  /*
    ∃ value, left, right, L, R, lv, ll, lr, LL, LR.
          ( node↦value,lr,right
          ∗ Tree(right, R)
          ∗ left↦lv,ll,node
          ∗ Tree(ll, LL)
          ∗ Tree(lr, LR) )
       && LL ∪ {lv} ∪ LR ∪ {value} ∪ R == S
       && (∀ v∈LL. v < lv)
       && (∀ v∈LR. v > lv && v < value)
       && (∀ v∈R. v > value) )
  */
  return left;
}
