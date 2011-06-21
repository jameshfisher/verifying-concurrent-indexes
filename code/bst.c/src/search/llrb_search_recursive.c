#ifndef LLRB_SEARCH_RECURSIVE_C_
#define LLRB_SEARCH_RECURSIVE_C_

bool llrb_search(pLLRBNode node, int value) {
  // Given a node in the tree representing some set S,
  // return a boolean representing whether `value` is in S.
  //
  // Approach:
  //    if(node is empty) { S is empty, so value is not S, so return false }
  //    else if (node->value == value) { value is definitely in S, so return true }
  //    else { `value` is only in S if it is in `node->left` or `node->right` (as appropriate),
  //            so return the value returned by recursively searching that subtree. }

  // Our precondition is that `node` is some Tree representing the set S,
  // though we don't know its height or color:
  //
  //// ∃ pH, pC.
  ////   Tree(node, S, pH, pC)

  bool o;

  // We are going to return `o`.
  // Therefore `o` is going to assert whether `value` is in S.
  // i.e., our postcondition will be: `o ↔ (value ∈ S)`

  if (node == NULL) {  // NODE_IS_NULL if-branch

    //// node == NULL && ∃ pH, pC. Tree(node, S, pH, pC)

    //// ∃ pH, pC. Tree(node, S, pH, pC) && node == NULL      // Bring `node == NULL` inside ∃

    //// ∃ pH, pC. EmptyTree(node, S, pH, pC)                 // NULL_IS_EMPTY

    //// ∃ pH, pC. EmptyTree(node, S, pH, pC) && S == {}      // EMPTY_TREE_IS_EMPTY_SET

    //// S == {}  &&  ∃ pH, pC. EmptyTree(node, S, pH, pC)    // Take S out of quantification

    //// value ∉ {}                                           // Axiom instance

    //// value ∉ S                                            // S == {}

    o = false;

    //// o == false  &&  value ∉ S                            // Assignment

    //// o == false  &&  (value ∈ S) == false                 // Desugaring

    //// o ↔ value ∈ S                                        // false ↔ false (TODO: what is this rule?)

  } // end of NODE_IS_NULL if-branch

  else {  // NODE_IS_NULL else-branch

    //// node != NULL  &&  ∃ pH, pC. Tree(node, S, pH, pC)

    //// ∃ pH, pC. NonEmptyTree(node, S, pH, pC)              // NON_NULL_IS_NON_EMPTY

    //// node↦-,-,-,-                                         // node->value is allocated,
                                                              // so following test does not segfault

    if (node->value == value) {   // FOUND_VALUE if-branch

      //// ∃ pH, pC. NonEmptyTree(node, S, pH, pC) && node->value == value

      // value ∈ S                                            // VALUE_IN_SET

      o = true;

      //// o == true  &&  (value ∈ S) == true                 // Assignment

      //// o ↔ (value ∈ S)                                    // true ↔ true (TODO: what is this rule?)

    } // end of FOUND_VALUE if-branch

    else {  // FOUND_VALUE else-branch

      //// ∃ pH, pC. NonEmptyTree(node, S, pH, pC)            // negated FOUND_VALUE if-test
      //// && node->value != value

      if (value < node->value) {  // LESS_THAN if-branch
        //// ∃ pH, pC. NonEmptyTree(node, S, pH, pC)
        //// && node->value != value
        //// && value < node->value                           // affirmed LESS_THAN if-test

        //// ∃ pH, pC. NonEmptyTree(node, S, pH, pC)          // &&E;  != adds no extra information (TODO: name?)
        //// && value < node->value

        //// ∃ l, L, h, c. node↪-,l,-,-                       // LESS_THAN_ONLY_IN_LEFT
        //// ∗ Tree(l, L, h, c) ∗ true
        //// && (value ∈ S) ↔ (value ∈ L)

        o = llrb_search(node->left, value);

        //// ∃ l, L, h, r. node↪-,l,-,-                       // Specification of llrb_search() (TODO: is it that simple?)
        //// ∗ Tree(l, L, h, c) ∗ true
        //// && (value ∈ S) ↔ (value ∈ L)
        //// && o ↔ (value ∈ L)

        //// ∃ l, L. o ↔ (value ∈ S)                          // implication; &&E, ∗E??

        // o ↔ (value ∈ S)                                    // vacuous ∃

      } // end of LESS_THAN if-branch

      else {  // LESS_THAN else-branch

        //// ∃ pH, pC. NonEmptyTree(node, S, pH, pC)
        //// && node->value != value
        //// && ¬(value < node->value)                        // negated LESS_THAN if-test

        //// ∃ pH, pC. NonEmptyTree(node, S, pH, pC)
        //// && node->value != value
        //// && value >= node->value                          // TODO: what is this rule?

        // (mostly symmetrical)

        //// ∃ pH, pC. NonEmptyTree(node, S, pH, pC)          // &&E
        //// && value > node->value

        //// ∃ r, R. node↪-,-,r,-                             // GREATER_THAN_ONLY_IN_RIGHT
        //// ∗ Tree(r, R) ∗ true
        //// && (value ∈ S) ↔ (value ∈ R)

        o = llrb_search(node->right, value);

        //// ∃ r, R. node↪-,-,r,-                             // Specification of llrb_search()
        //// ∗ Tree(r, R) ∗ true
        //// && (value ∈ S) ↔ (value ∈ R)
        //// && o ↔ (value ∈ R)

        //// ∃ r, R. o ↔ (value ∈ S)                          // implication; &&E, ∗E??

        // o ↔ (value ∈ S)                                    // vacuous ∃

      } // end of LESS_THAN else-branch

      // o ↔ (value ∈ S)                                      // final conditions of LESS_THAN

    } // end of FOUND_VALUE else-branch

    // o ↔ (value ∈ S)                                        // final conditions of FOUND_VALUE

  } // end of NODE_IS_NULL else-branch

  // o ↔ (value ∈ S)                                          // final conditions of NODE_IS_NULL

  return o;
}

#endif  // LLRB_SEARCH_RECURSIVE_C_
