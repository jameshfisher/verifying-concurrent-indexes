#ifndef LLRB_SEARCH_ITERATIVE_C_
#define LLRB_SEARCH_ITERATIVE_C_

bool llrb_search(pLLRBNode node, int value) {
  // Given a node in the tree representing some set S,
  // return a boolean representing whether `value` is in S.
  //
  // Approach:
  //    Follow the path from node to the position that `value` would be in if present.
  //    If we end with a null leaf, value cannot be present; otherwise it is.
  //
  //    while (`node` is not empty, but does not contain `value` at the root)
  //      set `node` to the one subchild that `value` could be in.
  //    if (`node` is not empty)
  //      we must have exited the loop because it contains `value` at the root,
  //      so `value` is in the set,
  //      so return true.
  //    else
  //      we followed the path to a null leaf where `value` would have to be if it was in the set,
  //      but it isn't there, so it's not in the set;
  //      so return false.

  // Our precondition is that `node` is some Tree representing the set S:
  //
  //// ∃ pH, pC.  Tree(node, S, pH, pC)

  pLLRBNode n = node;

  //// ∃ pH, pC.  Tree(n, S, pH, pC)         // Assignment


  // The invariant for our while loop is that
  // `n` represents a set Q such that value ∈ S if and only if value ∈ Q.
  // That is,
  //      ∃ pH, pC, Q.  Tree(n, Q, pH, pC) && value ∈ Q ↔ value ∈ S

  // The size of Q strictly decreases with each iteration,
  // and the size of Q >= 0, as the empty set is the smallest possible set.
  // This shows that the loop terminates.

  //// value ∈ S ↔ value ∈ S                                              // TODO: what is this

  //// value ∈ S ↔ value ∈ S  &&  ∃ pH, pC. Tree(n, S, pH, pC)            // &&I

  //// ∃ pH, pC.  Tree(n, S, pH, pC) && value ∈ S ↔ value ∈ S             // Bring into ∃

  //// ∃ pH, pC, Q.  Tree(n, Q, pH, pC) && value ∈ Q ↔ value ∈ S          // ∃I (SEARCHING loop invariant)

  while (n != NULL && n->value != value) { // SEARCHING

    //// ∃ pH, pC, Q.  Tree(n, Q, pH, pC) && value ∈ Q ↔ value ∈ S        // SEARCHING invariant.
    //// && n != NULL && n->value != value                                // Affirm SEARCHING while-condition.

    //// ∃ pH, pC, Q.  NonEmptyTree(n, Q, pH, pC)                         // NON_NULL_IS_NON_EMPTY
    //// && value ∈ Q ↔ value ∈ S
    //// && n->value != value

    //// node↦-,-,-,-                                                     // node->value is allocated,
                                                                          // so following test does not segfault

    if (value < n->value) { // LESS_THAN if-branch

      //// ∃ pH, pC, Q.  NonEmptyTree(n, Q, pH, pC)
      //// && value ∈ Q ↔ value ∈ S
      //// && n->value != value
      //// && value < n->value                                            // affirm LESS_THAN if-test

      //// ∃ pH, pC, Q.  NonEmptyTree(n, Q, pH, pC)                       // &&E; discard !=
      //// && value ∈ Q ↔ value ∈ S
      //// && value < n->value

      //// ∃ l, L, h, c. n↪-,l,-,-                                        // LESS_THAN_ONLY_IN_LEFT
      //// ∗ Tree(l, L, h, c) ∗ true
      //// && (value ∈ S) ↔ (value ∈ L)

      n = n->left;

      //// ∃ L, h, c. true ∗ Tree(n, L, h, c) ∗ true                      // Assignment
      //// && (value ∈ S) ↔ (value ∈ L)

      //// ∃ L, h, c. Tree(n, L, h, c)                                    // Discard parts of the heap (TODO: ???)
      //// && (value ∈ S) ↔ (value ∈ L)

      //// ∃ Q, pH, pC. Tree(n, Q, pH, pC) && (value ∈ Q) ↔ (value ∈ S)   // rearranging; alpha-renaming (TODO: this is lambda calc. terminology; what is it called in predicate logic?)

    } // end of LESS_THAN if-branch

    else {  // LESS_THAN else-branch

      //// ∃ pH, pC, Q.  NonEmptyTree(n, Q, pH, pC)
      //// && value ∈ Q ↔ value ∈ S
      //// && n->value != value
      //// && ¬(value < n->value)                                         // negate LESS_THAN if-test

      //// ∃ pH, pC, Q.  NonEmptyTree(n, Q, pH, pC)
      //// && value ∈ Q ↔ value ∈ S
      //// && value > n->value                                            // ¬(<) implies >=.  Then (>= && !=) implies >

      //// ∃ r, R, h, c. n↪-,-,r,-                                        // GREATER_THAN_ONLY_IN_RIGHT
      //// ∗ Tree(r, R, h, c) ∗ true
      //// && (value ∈ S) ↔ (value ∈ R)

      n = n->right;

      //// ∃ R, h, c. true ∗ Tree(n, R, h, c) ∗ true                      // Assignment
      //// && (value ∈ S) ↔ (value ∈ R)

      //// ∃ R, h, c. Tree(n, R, h, c)                                    // Discard parts of heap (???)
      //// && (value ∈ S) ↔ (value ∈ R)

      //// ∃ Q, pH, pC. Tree(n, Q, pH, pC) && (value ∈ Q) ↔ (value ∈ S)   // rearranging; renaming

    } // end of LESS_THAN else-branch

    //// ∃ Q, pH, pC. Tree(n, Q, pH, pC) && (value ∈ Q) ↔ (value ∈ S)     // final conditions of LESS_THAN

  } // end of SEARCHING loop

  //// ∃ Q, pH, pC. Tree(n, Q, pH, pC) && (value ∈ Q) ↔ (value ∈ S)       // SEARCHING invariant.
  //// && ¬(n != NULL && n->value != value)                               // negated SEARCHING condition.

  //// ∃ Q, pH, pC. Tree(n, Q, pH, pC) && (value ∈ Q) ↔ (value ∈ S)
  //// && (n == NULL || n->value == value)                                // De Morgan

  bool o;
  // We're going to `return o`.  That is,
  // we now make o satisfy our postcondition, namely `o ↔ (value ∈ S)`.

  if (n != NULL) {  // NOT_NULL if-branch

    //// ∃ Q, pH, pC. Tree(n, Q, pH, pC) && (value ∈ Q) ↔ (value ∈ S)
    //// && (n == NULL || n->value == value)
    //// && (n != NULL)                                                   // affirm NOT_NULL if-test

    //// ∃ Q, pH, pC. Tree(n, Q, pH, pC) && (value ∈ Q) ↔ (value ∈ S)     // ||E, &&E
    //// && n != NULL && n->value == value

    //// ∃ Q, pH, pC. NonEmptyTree(n, Q, pH, pC)                          // NON_NULL_IS_NON_EMPTY
    //// && (value ∈ Q) ↔ (value ∈ S)
    //// && n->value == value

    //// && (value ∈ Q) ↔ (value ∈ S)                                     // VALUE_IN_SET; &&E, vacuous ∃
    //// && (value ∈ Q)

    //// value ∈ S                                                        // MP; &&E

    o = true;

    // o && value ∈ S                                                     // Assignment

    // o ↔ (value ∈ S)                                                    // ↔I ?

  } // end of NOT_NULL if-branch

  else { // NOT_NULL else-branch

    //// ∃ Q, pH, pC. Tree(n, Q, pH, pC) && (value ∈ Q) ↔ (value ∈ S)
    //// && (n == NULL || n->value == value)
    //// && ¬(n != NULL)                                                  // negate NOT_NULL if-test

    //// ∃ Q, pH, pC. Tree(n, Q, pH, pC) && (value ∈ Q) ↔ (value ∈ S)
    //// && n == NULL                                                     // ¬¬E; discard `|| n->value == value`

    //// ∃ Q, pH, pC. EmptyTree(n, Q, pH, pC)                             // NULL_IS_EMPTY
    //// && (value ∈ Q) ↔ (value ∈ S)

    //// ∃ Q, pH, pC. EmptyTree(n, Q, pH, pC) && Q == {}                  // EMPTY_TREE_IS_EMPTY_SET
    //// && (value ∈ Q) ↔ (value ∈ S)

    //// value ∉ {}                                                       // Axiom instance

    //// ∃ Q. (value ∈ Q) ↔ (value ∈ S)                                   // equality; &&E;  vacuous ∃
    ////      && value ∉ Q

    //// ∃ Q. value ∉ S                                                   // MP; &&E

    //// ¬(value ∈ S)                                                     // vacuous ∃; desugar

    o = false;

    // ¬o && ¬(value ∈ S)                                                 // Assignment

    // o ↔ (value ∈ S)                                                    // ??

  } // end of NOT_NULL else-branch

  // o ↔ (value ∈ S)                                                      // final condition of NOT_NULL

  return o;
}

#endif  // LLRB_SEARCH_ITERATIVE_C_
