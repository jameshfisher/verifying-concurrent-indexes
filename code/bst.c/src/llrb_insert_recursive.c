#ifndef LLRB_INSERT_RECURSIVE_C_
#define LLRB_INSERT_RECURSIVE_C_

pNode insert_subtree(pNode node, int value) {
  pNode o;

  if (node == NULL) {
    o = newNode(value);
  }
  else {

    if (red(node->left) && red(node->right)) {
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
        node->left = insert_subtree(node->left, value);
      }
      else {
        node->right = insert_subtree(node->right, value);
      }


      if (red(node->right) && !red(node->left)) {
        node = rotateLeft(node);
      }
      else {
      }

      if (red(node->left) && red(node->left->left)) {
        node = rotateRight(node);
      }
      else {
      }

      o = node;
    }

  }

  return o;
}

pNode insert(pNode node, int value) {
  pNode o = insert_subtree(node, value);
  o->color = 1;
  return o;
}

#endif  // LLRB_INSERT_RECURSIVE_C_
