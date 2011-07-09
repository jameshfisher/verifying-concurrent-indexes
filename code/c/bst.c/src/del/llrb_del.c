#ifndef LLRB_DEL_C_
#define LLRB_DEL_C_

#include "../llrb.h"


pLLRBNode llrb_move_red_left(pLLRBNode node) {
  // !llrb_red(node->left) && !llrb_red(node->left->left)
  // i.e., OBT or RT
  node = llrb_color_flip(node);
  /* BT(h) but left and right might be RV'd */
  if (llrb_red(node->right->left)) {
    /* RV on node->right */
    node->right = llrb_rotate_right(node->right);
    /* two reds down the right, one down left */
    node = llrb_rotate_left(node);
    /* two down left, one down right */
    node = llrb_color_flip(node);
    /* left is FBT, right is OBT */
  }
  return node;
}


pLLRBNode llrb_move_red_right(pLLRBNode node) {
  printf("Moving red right\n");
  // !llrb_red(node->right) && !llrb_red(node->right->left)
  // i.e, right is OBT
  node = llrb_color_flip(node);
  printf("Flipped colors\n");
  if (llrb_red(node->right->left)) {
    /* red left->left */
    printf("Rotating right\n");
    node = llrb_rotate_right(node);
    printf("Rotated right\n");
    /* red left */
    node = llrb_color_flip(node);
    printf("Colop flipped again\n");
  }
  return node;
}


pLLRBNode llrb_del_min(pLLRBNode node, int * val) {
  printf("Deleting min\n");
  pLLRBNode o;
  if (node->left == NULL) {
    *val = node->value;
    free(node);
    o = NULL;
  }
  else {
    if (!llrb_red(node->left) && !llrb_red(node->left->left)) {
      node = llrb_move_red_left(node);
    }
    node->left = llrb_del_min(node->left, val);
    o = llrb_fix_up(node);
  }
  return o;
}


pLLRBNode llrb_del_recursive(pLLRBNode node, int value) {
  /* node is BT or RT */
  printf("Deleting %d from\n", value);
  llrb_print_node(node, 0);
  

  if (node == NULL) {
    return NULL;
  }
  if (value < node->value) {
    printf("Deleting from lesser\n");
    if (node->left == NULL) {
      return node;
    }
    else {
      if (!llrb_red(node->left) && !llrb_red(node->left->left)) {
        node = llrb_move_red_left(node);
      }
      node->left = llrb_del_recursive(node->left, value);
    }
  }

  else {    
    if (value == node->value) {
      node->right = llrb_del_min(node->right, &(node->value));
    }
    else {
      // node->value < value
      if(node->right != NULL) {
        printf("Deleting from greater\n");
        printf("Right tree:\n");
        llrb_print_node(node->right, 0);

        if (llrb_red(node->left)) {
          printf("Rotating right\n");
          node = llrb_rotate_right(node);
          printf("Rotated right\n");
        }
        printf("After possible rotation\n");

        if (value == node->value && node->right == NULL) {
          return NULL;
        }
        printf("After possible return NULL\n");

        if (!llrb_red(node->right) && !llrb_red(node->right->left)) {

          printf("Moving red right\n");
          node = llrb_move_red_right(node);
          printf("Moved right\n");
        }
        printf("After possible move red right\n");

        node->right = llrb_del_recursive(node->right, value);
      }
    }
  }

  return llrb_fix_up(node);
}

pLLRBNode llrb_del(pLLRBNode node, int value) {
  printf("deleting at root\n");
  if (node == NULL) {
    return NULL;
  }
  if (node->right == NULL) {
    if (value == node->value) {
      free(node);
      return NULL;
    }
    else {
      return node;
    }
  }
  else {
    // node-right is non-null black
    pLLRBNode o = llrb_del_recursive(node, value);
    o->color = 1;
    return o;
  }
}

#endif  // LLRB_DEL_C_
