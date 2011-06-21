
//-----------------------------------------------------------------//
//                           iterative                             //
//-----------------------------------------------------------------//

pNode insert(pNode node, int value) {
  // Tree(node, S)

  pNode par = NULL;
  pNode n = node;

  // Tree(node, S) && Tree(n, S)

  while (n && n->value != value) {
    // ∃Q. NonEmptyTree(n, Q)
    par = n;
    // ∃Q. NonEmptyTree(par, Q) && NonEmptyTree(n, Q)

    if (value < n->value) {
      n = n->left;
    }
    else {
      // value >= n->value
      n = n->right;
    }
    // ∃Q. NonEmptyTree(par, Q) && Tree(n, T)
    // && value ∈ T ↔ value ∈ Q ↔ value ∈ S
  }
  // !n || n->value == value

  pNode o;

  if (n) {
    // n->value == value
    o = node;
  }
  else {
    if (!par) {
      // S == {}
      o = newNode(value);
      // NonEmptyTree(o, {value})
      // NonEmptyTree(o, {} ∪ {value})
      // NonEmptyTree(o, S ∪ {value})
    }
    else {
      if (value < par->value) {
        par->left = newNode(value);
        // NonEmptyTree(node, S ∪ {value})
      }
      else {
        if (value > par->value) {
          par->right = newNode(value);
          // NonEmptyTree(node, S ∪ {value})
        }
        else {
          // NonEmptyTree(node, S ∪ {value})
        }
        // NonEmptyTree(node, S ∪ {value})
      }

      // NonEmptyTree(node, S ∪ {value})
      o = node;
      // NonEmptyTree(o, S ∪ {value})
    }
  }

  // NonEmptyTree(o, S ∪ {value})
  // Tree(o, S ∪ {value})

  return o;
}

