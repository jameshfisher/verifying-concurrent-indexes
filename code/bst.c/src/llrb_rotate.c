
//-----------------------------------------------------------------//
//                             rotate                              //
//-----------------------------------------------------------------//


pNode rotateRight(pNode node) {
  pNode left = node->left;
  node->left = left->right;
  left->right = node;

  left->color = node->color;
  node->color = 0;

  return left;
}

pNode rotateLeft(pNode node) {
  pNode right = node->right;
  node->right = right->left;
  right->left = node;

  right->color = node->color;
  node->color = 0;

  return right;
}


