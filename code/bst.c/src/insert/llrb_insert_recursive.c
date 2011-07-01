#ifndef LLRB_INSERT_RECURSIVE_C_
#define LLRB_INSERT_RECURSIVE_C_

#include "../llrb.h"

pLLRBNode llrb_color_flip(pLLRBNode node) {
  pLLRBNode o = node;
  // node is RT, ONBT, HFBT, or FBT
  if (llrb_red(o->left) && llrb_red(o->right)) {
    o->color = !o->color;
    o->left->color = !o->left->color;
    o->right->color = !o->right->color;
  }
  return o;
}


pLLRBNode llrb_split(pLLRBNode node) {
  // pre: node non-null; node->left non-null
  if (llrb_red(node->left) && llrb_red(node->left->left)) {
    node = llrb_rotate_right(node);
  }
  // post: if was RRR, now FBT
  return node;
}


pLLRBNode llrb_fix_up(pLLRBNode node) {
  if (llrb_red(node->right) && !llrb_red(node->left)) {
    node = llrb_rotate_left(node);
  }

  node = llrb_split(node);
  node = llrb_color_flip(node);
  return node;
}

pLLRBNode llrb_insert_subtree(pLLRBNode node, int value) {
  pLLRBNode o;

  if (node == NULL) {
    o = llrb_new_node(value);
  }
  else {

    if (node->value == value) {
      o = node;
      // o is RT, ONBT, or HFBT
    }

    else {
      
      if (value < node->value) {
        node->left = llrb_insert_subtree(node->left, value);

        o = llrb_split(node);
        o = llrb_color_flip(o);
      }

      else {
        node->right = llrb_insert_subtree(node->right, value);
        if(llrb_red(node->right) && llrb_red(node->right->left)) {
          printf("LLRT in right.\n"); exit(0);
        }

        o = llrb_fix_up(node);
      }
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
