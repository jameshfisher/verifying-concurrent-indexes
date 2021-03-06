#ifndef IS_LLRB_C_
#define IS_LLRB_C_

#include <stdbool.h>

#include "../llrb.h"

typedef enum LLRBNodeType { RED_LLRB, BLACK_LLRB, INVALID_LLRB } LLRBNodeType;

typedef struct LLRBNodeInfo {
  LLRBNodeType t;
  int h;
} LLRBNodeInfo;

LLRBNodeInfo llrbNodeInfo(pLLRBNode node) {
  LLRBNodeInfo o;

  if (node == NULL) {
    o = (LLRBNodeInfo){BLACK_LLRB, 0};
  }
  else {
    LLRBNodeInfo left = llrbNodeInfo(node->left);
    LLRBNodeInfo right = llrbNodeInfo(node->right);
    if (
         left.t == INVALID_LLRB || right.t == INVALID_LLRB
      || left.h != right.h
      || right.t == RED_LLRB
      || (node->color == 0 && left.t == RED_LLRB)
      ) {
      o = (LLRBNodeInfo){INVALID_LLRB, 0};
    }
    else {
      if (node->color == 0) {
        o = (LLRBNodeInfo){ RED_LLRB, left.h };
      }
      else {
        o = (LLRBNodeInfo){ BLACK_LLRB, left.h + 1 };
      }
    }
  }

  return o;
}


LLRBNodeInfo llrbNodeInfo234(pLLRBNode node) {
  LLRBNodeInfo o;

  if (node == NULL) {
    o = (LLRBNodeInfo){BLACK_LLRB, 0};
  }
  else {
    LLRBNodeInfo left = llrbNodeInfo234(node->left);
    LLRBNodeInfo right = llrbNodeInfo234(node->right);
    if (
         left.t == INVALID_LLRB || right.t == INVALID_LLRB
      || left.h != right.h
      || (right.t == RED_LLRB && left.t != RED_LLRB)
      || (node->color == 0 && (left.t == RED_LLRB || right.t == RED_LLRB))
      ) {
      o = (LLRBNodeInfo){INVALID_LLRB, 0};
    }
    else {
      if (node->color == 0) {
        o = (LLRBNodeInfo){ RED_LLRB, left.h };
      }
      else {
        o = (LLRBNodeInfo){ BLACK_LLRB, left.h + 1 };
      }
    }
  }

  return o;
}


bool isLLRB(pLLRBNode node) {
  LLRBNodeInfo info = llrbNodeInfo(node);
  return info.t == BLACK_LLRB;
}

void llrb_print_node_info(LLRBNodeInfo info) {
  switch(info.t) {
    case RED_LLRB:   printf("Red-rooted LLRB; height %d", info.h); break;
    case BLACK_LLRB: printf("Black-rooted LLRB; height %d", info.h); break;
    case INVALID_LLRB: printf("Invalid LLRB"); break;
  }
}

#endif  // IS_LLRB_C_
