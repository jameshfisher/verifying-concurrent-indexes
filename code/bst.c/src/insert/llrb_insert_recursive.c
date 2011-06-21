#ifndef LLRB_INSERT_RECURSIVE_C_
#define LLRB_INSERT_RECURSIVE_C_

#include "../llrb.h"

pLLRBNode llrb_insert_subtree(pLLRBNode node, int value) {
  pLLRBNode o;

  if (node == NULL) {
    o = llrb_new_node(value);
  }
  else {

    if (llrb_red(node->left) && llrb_red(node->right)) {
      node->color = !node->color;
      node->left->color = !node->left->color;
      node->right->color = !node->right->color;
    }
    else {
    }

    if (node->value == value) {
      o = node;
    }
    else {
      
      if (value < node->value) {
        node->left = llrb_insert_subtree(node->left, value);
      }
      else {
        node->right = llrb_insert_subtree(node->right, value);
      }


      if (llrb_red(node->right) && !llrb_red(node->left)) {
        node = llrb_rotate_left(node);
      }
      else {
      }

      if (llrb_red(node->left) && llrb_red(node->left->left)) {
        node = llrb_rotate_right(node);
      }
      else {
      }

      o = node;
    }

  }

  return o;
}

pLLRBNode llrb_insert(pLLRBNode node, int value) {
  pLLRBNode o = llrb_insert_subtree(node, value);
  o->color = 1;
  return o;
}

#endif  // LLRB_INSERT_RECURSIVE_C_
