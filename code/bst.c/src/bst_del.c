
//-----------------------------------------------------------------//
//                              del                                //
//-----------------------------------------------------------------//


/**
  Aux procedure.
  Remove the maximum value from the set represented by *pnode, and return that value.
  Precondition: *pnode is non-empty -- what is the maximum member of the empty set?
 */
int delMax(pNode * pnode) {
  // pnode↦node ∗ NonEmptyTree(node, S)

  /* pnode↦node ∗
     (∃ w, l, r, L, R. ( node↦w,l,r
                       ∗ Tree(l, L)
                       ∗ Tree(r, R) )
                      && L ∪ {w} ∪ R == S
                      && (∀ v∈L. v < w)
                      && (∀ v∈R. v > w) )
  */

  int max;

  if ((*pnode)->right) {

    /* pnode↦node ∗
       (∃ w, l, r, L, R. ( node↦w,l,r
                         ∗ Tree(l, L)
                         ∗ NonEmptyTree(r, R) )
                        && L ∪ {w} ∪ R == S
                        && (∀ v∈L. v < w)
                        && (∀ v∈R. v > w) )
    */
    max = delMax(&((*pnode)->right));

    /* pnode↦node ∗
       (∃ w, l, r, L, R. ( node↦w,l,r
                         ∗ Tree(l, L)
                         ∗ Tree(r, R-{max}) )
                        && L ∪ {w} ∪ R-{max} == S-{max}
                        && (∀ v∈L. v < w)
                        && (∀ v∈(R-{max}). v > w) )
       && ∀ x∈S. x <= max.
    */

    /* ( pnode↦node
       ∗ Tree(node, S-{max}) )
      && ∀ x∈S. x <= max.
    */
  }
  else {
    /* pnode↦node ∗
       (∃ max, l, r, L. ( node↦max,l,r
                        ∗ Tree(l, L)
                        ∗ EmptyTree(r, {}) )
                       && L ∪ {max} == S
                       && (∀ v∈L. v < max) )
    */
    max = (*pnode)->value;

    /* pnode↦node ∗
       (∃ l, r, L. ( node↦max,l,r
                   ∗ Tree(l, L)
                   ∗ EmptyTree(r, {}) )
                  && L ∪ {max} == S
                  && (∀ v∈L. v < max) )
    */
    // ... ∀ x∈S. x <= max.

    pNode node = (*pnode);
    (*pnode) = (*pnode)->left;

    //   pnode↦node
    // ∗ node↦max,l,r
    // ∗ EmptyTree(r, {})
    // ∗ ∃ L. Tree(node, L) && L ∪ {max} == S

    //   pnode↦node
    // ∗ node↦max,l,r
    // ∗ emp
    // ∗ ∃ L. Tree(node, L) && L ∪ {max} == S

    //   pnode↦node
    // ∗ node↦max,l,r
    // ∗ Tree(node, S-{max})

    free(node);

    //   pnode↦node
    // ∗ Tree(node, S-{max})

    /* ( pnode↦node
       ∗ Tree(node, S-{max}) )
      && ∀ x∈S. x <= max.
    */
  }

  /* ( pnode↦node
     ∗ Tree(node, S-{max}) )
    && ∀ x∈S. x <= max.
  */

  return max;
}

/**
  Aux.  Given pointer to non-null node,
  return pointer to tree with that node removed.
 */
pNode delNode(pNode node) {
  /*
  (∃ value, left, right, L, R. ( node↦value,left,right
                               ∗ Tree(left, L)
                               ∗ Tree(right, R) )
                              && L ∪ {value} ∪ R == S
                              && (∀ v∈L. v < value)
                              && (∀ v∈R. v > value) ).
  */

  pNode o;

  if (!node->left) {
    /*
    (∃ value, left, right, R. ( node↦value,left,right
                              ∗ EmptyTree(left, {})
                              ∗ Tree(right, R) )
                             && {value} ∪ R == S
                             && (∀ v∈R. v > value) ).
    */
    // ... R == S-{value} ...

    o = node->right;
    /*
    (∃ value, left. ( node↦value,left,o
                    ∗ Tree(o, S-{value}) ).
    */

    free(node);

    // Tree(o, S-{value}).
  }
  else {
    /*
    (∃ value, left, right, L, R. ( node↦value,left,right
                                 ∗ NonEmptyTree(left, L)
                                 ∗ Tree(right, R) )
                                && L ∪ {value} ∪ R == S
                                && (∀ v∈L. v < value)
                                && (∀ v∈R. v > value) ).
    */
    if (!node->right) {
      /*
      (∃ value, left, right, L. ( node↦value,left,right
                                ∗ NonEmptyTree(left, L)
                               && L ∪ {value} == S
                               && (∀ v∈L. v < value) ).
      */
      // ... L = S-{value}

      o = node->left;

      /*
      (∃ value, right. ( node↦value,o,right
                       ∗ NonEmptyTree(o, S-{value}) ).
      */
      free(node);
      // NonEmptyTree(o, S-{value}).
      // Tree(o, S-{value}).
    }
    else {
      /*
      (∃ value, left, right, L, R. ( node↦value,left,right
                                   ∗ NonEmptyTree(left, L)
                                   ∗ NonEmptyTree(right, R) )
                                  && L ∪ {value} ∪ R == S
                                  && (∀ v∈L. v < value)
                                  && (∀ v∈R. v > value) ).
      */

      int max = delMax(&(node->left));

      /*
      (∃ value, left, right, L, R. ( node↦value,left,right
                                   ∗ Tree(left, L-{max})
                                   ∗ NonEmptyTree(right, R) )
                                  && L-{max} ∪ {value} ∪ R == S-{max}
                                  && (∀ v∈L. v < value)
                                  && (∀ v∈R. v > value) 
                                  && ∀ x∈L. x <= max ).
      */

      node->value = max;

      /*
      (∃ left, right, L, R. ( node↦max,left,right
                            ∗ Tree(left, L-{max})
                            ∗ NonEmptyTree(right, R) )
                           && L-{max} ∪ {max} ∪ R == S-{value}
                           && (∀ v∈L. v < max)
                           && (∀ v∈R. v > max) 
                           && ∀ x∈L. x <= max ).
      */

      // NonEmptyTree(node, S-{value}).
      // Tree(node, S-{value}).

      o = node;
    }
  }

  // Tree(o, S-{value}).

  return o;
}


/**
  Remove `value` from the set.
 */
pNode del(pNode node, int value) {

  // Tree(node, S)

  pNode o;

  if (!node) {
    // EmptyTree(node, S) && S == {}
    // {}-{value} = {}
    // S-{value} = {}
    o = node;
    // EmptyTree(o, {})
    // Tree(o, {})
    // Tree(o, S-{value})
  }
  else {
    // NonEmptyTree(node, S)
    if (node->value == value) {
      /*
        (∃ left, right, L, R. ( n↦value,left,right
                              ∗ Tree(left, L)
                              ∗ Tree(right, R) )
                             && L ∪ {value} ∪ R == S
                             && (∀ v∈L. v < value)
                             && (∀ v∈R. v > value) ).
      */
      o = delNode(node);
      // Tree(o, S-{value})
    }
    else {
      /*
        (∃ w, left, right, L, R. ( n↦w,left,right
                                 ∗ Tree(left, L)
                                 ∗ Tree(right, R) )
                                && L ∪ {w} ∪ R == S
                                && (∀ v∈L. v < w)
                                && (∀ v∈R. v > w)
                                && value ≠ w ).
      */
      if (value < node->value) {
        /*
          (∃ w, left, right, L, R. ( n↦w,left,right
                                   ∗ Tree(left, L)
                                   ∗ Tree(right, R) )
                                  && L ∪ {w} ∪ R == S
                                  && (∀ v∈L. v < w)
                                  && (∀ v∈R. v > w)
                                  && value < w ).
        */
        node->left = del(node->left, value);
        /*
          (∃ w, left, right, L, R. ( n↦w,left,right
                                   ∗ Tree(left, L-{value})
                                   ∗ Tree(right, R) )
                                  && L-{value} ∪ {w} ∪ R == S-{value}
                                  && (∀ v∈L-{value}. v < w)
                                  && (∀ v∈R. v > w)
                                  && value < w ).
        */
        // value ∉ R && value ∉ {w} && value ∉ L-{value}
        // Tree(node, S-{value})
      }
      else {
        // (symmetrical)
        node->right = del(node->right, value);
        // Tree(node, S-{value})
      }
      // Tree(node, S-{value})
      o = node;
      // Tree(o, S-{value})
    }
    // Tree(o, S-{value})
  }

  // Tree(o, S-{value})

  return o;
}
