module rb.rotate;

import rb.node;

Node * single(Node * oldRoot, int dir) {

  // 3T(n, S, bh, dir)

  //        oldRoot, 3T(H, dir)
  //         /           \
  //        /             \
  //       /         newRoot, RT(dir, H-1)
  //      /               /       \
  //     /               /         \
  // sibling,       gc1,          gc2, 
  // BT(H-1)        BT(H-1)       BT(H-1)

  // Open predicates:
  //
  //          oldRoot, B
  //         /           \
  //        /             \
  //       /             newRoot, R
  //      /               /       \
  //     /               /         \
  // sibling,       gc1,          gc2, 
  // BT(H-1)        BT(H-1)       BT(H-1)

  Node * newRoot = oldRoot.c[!dir];
  oldRoot.c[!dir] = newRoot.c[dir];
  newRoot.c[dir] = oldRoot;

  newRoot.black = true;
  oldRoot.black = false;

  //            newRoot, B
  //             /          \
  //            /            \
  //     oldRoot, R           \
  //      /      \             \
  //     /        \             \
  // sibling,       gc1,          gc2, 
  // BT(H-1)        BT(H-1)       BT(H-1)

  // Close predicates:
  //
  //            newRoot, 3T(H, !dir)
  //             /          \
  //            /            \
  //   oldRoot, RT(H-1)       \
  //      /      \             \
  //     /        \             \
  // sibling,       gc1,          gc2, 
  // BT(H-1)        BT(H-1)       BT(H-1)

  // 3T(n, S, bh, !dir)

  return newRoot;
}

void single_ptr(Node ** oldRoot, int dir) {
  *oldRoot = single(*oldRoot, dir);
}


Node * dbl(Node * oldRoot, int dir) {

  // n↦v,c[0],c[1],?  ∗  BT(c[dir], L, h-1)  ∗  RVT(c[!dir], R, h-1, !dir)  && Compose(L, v, R, S)

  // rotate_double can be used to fix a red violation.
  // An analogy here is rearranging the elements of a four-node:
  //
  //      |                             |
  //      a---b---c     ==>         a---b---c
  //    /   |   |   \             /   |   |   \

  //          <=====dir======
  //    
  //             oldRoot, R/B
  //           /           \
  //          /             \
  //         /          child, RVT(!dir, h-1)
  //        /               /               \
  //       /               /                 \
  //      /          newRoot, RT(h-1)         \
  //     /           /             \           \
  //    /           /               \           \
  //  sib,        ggc1,           ggc2,         gc2,
  //  B(h-1)      BT(h-1)         BT(h-1)       BT(h-1)

  oldRoot.c[!dir] = single(oldRoot.c[!dir], !dir);
  Node * newRoot = single(oldRoot, dir);

  // 4T(n, S, h)

  //              ---newRoot, 4T(h)---
  //             /                    \
  //            /                      \
  //     oldRoot, RT(h-1)           child, RT(h-1)
  //     /         \                 /         \
  //    /           \               /           \
  //  sib,        ggc1,           ggc2,         gc2,
  //  B(h-1)      BT(h-1)         BT(h-1)       BT(h-1)

  return newRoot;
}

void dbl_ptr(Node ** oldRoot, int dir) {
  *oldRoot = dbl(*oldRoot, dir);
}

