#ifndef LLRB_H_
#define LLRB_H_

struct LLRBNode;
typedef struct LLRBNode LLRBNode;
typedef LLRBNode * pLLRBNode;

struct LLRBNode {
  int value;
  pLLRBNode left, right;
  int color;
};


// Tree(n, S, pColor, pHeight) =
//    EmptyTree(n, S, pColor, pHeight)
// || NonEmptyTree(n, S, pColor, pHeight).

// rbCond(color, pColor, height, pHeight) =
//    ( (  (color == RED   && height = pHeight)
//      || (color == BLACK && height = pHeight-1) )
//   && color == RED ==> pColor == BLACK
//   && pColor == RED ==> color == BLACK
//   && height > 0 ).

// EmptyTree(n, S, pColor, pHeight) =
//   (  emp
//   && n == NULL
//   && S == {}
//   && rbCond(BLACK, pColor, 1, pHeight)
//   ).

// Trivial rules on EmptyTree
// --------------------------
//
// EmptyTree(n, S, pC, pH)  ==>  emp        // EMPTY_TREE_IS_EMPTY_HEAP
// EmptyTree(n, S, pC, pH)  ==>  n == NULL  // EMPTY_TREE_IS_NULL_POINTER
// EmptyTree(n, S, pC, pH)  ==>  S == {}    // EMPTY_TREE_IS_EMPTY_SET


// NonEmptyTree(n, S, pColor, pHeight) =
//   (∃ value, left, right, color, height, L, R.
//     ( n↦value,left,right,color
//     ∗ Tree(left, L, color, height)
//     ∗ Tree(right, R, color, height) )
//    && L ∪ {value} ∪ R == S
//    && (∀ v∈L. v < value)
//    && (∀ v∈R. v > value)
//    && rbCond(color, pColor, height, pHeight) ).


// NonEmptyTree(n, S, pC, pH) ==> n != NULL   // NON_EMPTY_TREE_IS_NON_NULL_POINTER
// NonEmptyTree(n, S, pC, pH) ==> S != {}     // NON_EMPTY_TREE_IS_NON_EMPTY_SET



// NULL_IS_EMPTY
// -------------
//
// -1.      Tree(n, S, pC, pH) && n==NULL                                 assume
//  2.      Tree(n, S, pC, pH)                                            1, &&E
//  3.      n==NULL                                                       1, &&E
//  4.      EmptyTree(n, S, pC, pH) || NonEmptyTree(n, S, pC, pH)         2, definition of Tree
// -5.          NonEmptyTree(n, S, pC, pH)                                assume
//  6.          n != NULL                                                 NON_EMPTY_TREE_IS_NON_NULL_POINTER
//  7.      ¬(NonEmptyTree(n, S, pC, pH))                                 5, 3, 6, RAA
//  8.      EmptyTree(n, S, pC, pH)                                       4, 7, ||E
//  9.  Tree(n, S, pC, pH) && n==nil  ==>  EmptyTree(n, S, pC, pH)        1, 8, ==>I


// NON_NULL_IS_NON_EMPTY
// ---------------------
//
// (similar)


// VALUE_IN_SET
// ------------
//
//  1.      NonEmptyTree(node, S, pH, pC) && node->value == value         assume
//  2.      NonEmptyTree(node, S, pH, pC)                                 1, &&E
//  3.      node->value == value                                          1, &&E

//  4.      ∃ w, l, r, c, h, L, R.                                        2, definition of NonEmptyTree
//               node↦w,l,r,c
//             ∗ Tree(l, L, c, h)
//             ∗ Tree(r, R, c, h)
//            && L ∪ {w} ∪ R == S
//            && (∀ v∈L. v < w)
//            && (∀ v∈R. v > w)
//            && rbCond(c, pC, h, pH)

//  5.      ∃ w, L, R.                                                    4, &&E, ∗E??, ∃E
//               L ∪ {w} ∪ R == S

//  6.      ∃ (node->value), L, R.                                        5, renaming, TODO: pecularity of C; not sure how to deal with this
//               L ∪ {(node->value)} ∪ R == S

//  7.      ∃ L, R.  L ∪ {value} ∪ R == S                                 6, 3, equality
//  8.      ∃ L, R.  value ∈ S                                            7, some axiom
//  9.      value ∈ S                                                     8, vacuous ∃

// 10.  NonEmptyTree(node, S, pH, pC) && node->value == value             1, 9, ==>I
//        ==> value ∈ S


// LESS_THAN_ONLY_IN_LEFT
// ----------------------

//  1.      NonEmptyTree(node, S, pH, pC)                                 assume
//          && value < node->value

//  2.      NonEmptyTree(node, S, pH, pC)                                 &&E
//  3.      value < node->value                                           &&E
//  4.      value != node->value                                          3, some weakening
//  5.      value ∉ {(node->value)}                                       4, TODO: what is this

//  4.      ∃ (node->value), l, r, c, h, L, R.                            2, definition of NonEmptyTree, &&E, renaming??
//               node↦(node->value),l,r,c
//             ∗ Tree(l, L, c, h)
//             ∗ Tree(r, R, c, h)
//            && L ∪ {(node->value)} ∪ R == S
//            && (∀ v∈L. v < (node->value))
//            && (∀ v∈R. v > (node->value))

//  5.      ∃ ...                                                         // &&I
//            && value ∉ {(node->value)}

//  6.      ∃ ...                                                         // value does not satisfy restriction on set R; TODO: name
//            && value ∉ {(node->value)}
//            && value ∉ R

//  7.      ∃ ...                                                         // NOT_IN_SUBSET_THEN_MAYBE_IN_DIFFERENCE
//            && value ∉ {(node->value)}
//            && value ∉ R
//            && value∈S ↔ value∈L

//  8.      ∃ l, c, h, L.                                                 // &&E, heap-description weakening, 
//               node↦-,l,-,-
//             ∗ Tree(l, L, c, h)
//             ∗ true
//            && value∈S ↔ value∈L

//  9.    NonEmptyTree(node, S, pH, pC) && value < node->value            1, 8, ==>I
//      ==> ∃ l, c, h, L.
//               node↦-,l,-,- ∗ Tree(l, L, c, h) ∗ true
//            && value∈S ↔ value∈L


// NOT_IN_SUBSET_THEN_MAYBE_IN_DIFFERENCE
// TODO

#endif  // LLRB_H_
