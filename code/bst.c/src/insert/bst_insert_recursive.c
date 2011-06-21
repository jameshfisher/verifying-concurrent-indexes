
//-----------------------------------------------------------------//
//                           recursive                             //
//-----------------------------------------------------------------//

pNode insert(pNode node, int value) {
  // Tree(node, S)

  pNode o;

  if (!node) {
    // EmptyTree(node, S) && S == {}
    
    o = newNode(value);
    // Tree(o, {value})
    // Tree(o, {} ∪ {value})
    // Tree(o, S ∪ {value})
  }
  else {
    /* ∃ w, l, r, L, R. ( node↦w,l,r
                        ∗ Tree(l, L)
                        ∗ Tree(r, R) )
                       && L ∪ {w} ∪ R == S
                       && (∀ v∈L. v < w)
                       && (∀ v∈R. v > w) ). */
    if (node->value == value) {
      // Tree(node, S)
      /* ∃ l, r, L, R. ( node↦value,l,r
                       ∗ Tree(l, L)
                       ∗ Tree(r, R) )
                      && L ∪ {value} ∪ R == S
                      && (∀ v∈L. v < value)
                      && (∀ v∈R. v > value) ). */
      // S == S ∪ {value}
      // Tree(node, S ∪ {value})
      o = node;
      // Tree(o, S ∪ {value})
    }
    else {
      /* ∃ w, l, r, L, R. ( node↦w,l,r
                          ∗ Tree(l, L)
                          ∗ Tree(r, R) )
                         && L ∪ {w} ∪ R == S
                         && (∀ v∈L. v < w)
                         && (∀ v∈R. v > w)
                         && w ≠ value). */
      
      if (value < node->value) {
        /* ∃ w, l, r, L, R. ( node↦w,l,r
                            ∗ Tree(l, L)
                            ∗ Tree(r, R) )
                           && L ∪ {w} ∪ R == S
                           && (∀ v∈L. v < w)
                           && (∀ v∈R. v > w)
                           && w > value). */
        node->left = insert(node->left, value);
        /* ∃ w, l, r, L, R. ( node↦w,l,r
                            ∗ Tree(l, L ∪ {value})
                            ∗ Tree(r, R) )
                           && L ∪ {value} ∪ {w} ∪ R == S ∪ {value}
                           && (∀ v ∈ (L∪{value}). v < w)
                           && (∀ v ∈ R.           v > w)
                           && w > value). */
        // Tree(node, S ∪ {value})
      }
      else {
        // (symmetrical)
        node->right = insert(node->right, value);
        // Tree(node, S ∪ {value})
      }

      // Tree(node, S ∪ {value})
      o = node;
      // Tree(o, S ∪ {value})
    }

    // Tree(o, S ∪ {value})
  }

  // Tree(o, S ∪ {value})

  return o;
}
