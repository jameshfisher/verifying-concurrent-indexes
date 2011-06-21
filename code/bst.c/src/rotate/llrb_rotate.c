#ifndef LLRB_ROTATE_C_
#define LLRB_ROTATE_C_

#include "../llrb.h"

pLLRBNode llrb_rotate_right(pLLRBNode node) {
  pLLRBNode left = node->left;
  node->left = left->right;
  left->right = node;

  left->color = node->color;
  node->color = 0;

  return left;
}

pLLRBNode llrb_rotate_left(pLLRBNode node) {
  pLLRBNode right = node->right;
  node->right = right->left;
  right->left = node;

  right->color = node->color;
  node->color = 0;

  return right;
}

#endif  // LLRB_ROTATE_C_
