#ifndef LLRB_C_
#define LLRB_C_

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include "./llrb.h"



pNode newNode(int value) {
  // emp

  pNode node = malloc(sizeof(Node));

  // TODO
  node->color = 0;

  // ∃v,l,r. node↦v,l,r.

  node->value = value;

  // ∃l,r. node↦value,l,r.

  node->left = NULL;

  // ∃r. node↦value,NULL,r.

  node->right = NULL;

  // node↦value,NULL,NULL.
  // node↦value,NULL,NULL ∗ emp ∗ emp.
  // node↦value,NULL,NULL ∗ EmptyTree(NULL, {}) ∗ EmptyTree(NULL, {}).
  // ∃l,r. node↦value,l,r ∗ EmptyTree(l, {}) ∗ EmptyTree(r, {}).
  // ∃l,r. node↦value,l,r ∗ Tree(l, {}) ∗ Tree(r, {}).

  // ∃l,r, L, R. (node↦value,l,r
  //            ∗ Tree(l, L)
  //            ∗ Tree(r, R) )
  //           && L == {}
  //           && R == {}
  //           && L ∪ {value} ∪ R == {value}
  //           && (∀ v∈L. v < value)
  //           && (∀ v∈R. v > value).

  /* ∃ value, left, right, L, R. ( n↦value,left,right
                                 ∗ Tree(left, L)
                                 ∗ Tree(right, R) )
                                && L ∪ {value} ∪ R == {value}
                                && (∀ v∈L. v < value)
                                && (∀ v∈R. v > value). */

  // NonEmptyTree(node, {value})
  // Tree(node, {value})

  return node;
}

bool red(pNode node) {
  bool o;
  if (node == NULL) {
    o = false;
  }
  else {
    if (node->color == 0) {
      o = true;
    }
    else {
      o = false;
    }
  }
  return o;
}

#include "./rotate/llrb_rotate.c"

#include "./search/bst_search.c"

#include "./insert/llrb_insert.c"

#include "./del/bst_del.c"




#endif  // LLRB_C_
