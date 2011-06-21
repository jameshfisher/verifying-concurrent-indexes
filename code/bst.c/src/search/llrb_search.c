#ifndef LLRB_SEARCH_C_
#define LLRB_SEARCH_C_

#ifdef RECURSIVE
#  include "./llrb_search_recursive.c"
#else
#  include "./llrb_search_iterative.c"
#endif

#endif  // LLRB_SEARCH_C_
