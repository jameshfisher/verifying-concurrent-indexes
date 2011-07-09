/* rbtree(n, S, ..) */
e := search(n, val) {
  e := false;
  /* rbtree(n, S, ..)  and
     root = n  and 
     rootSet = S  and
     val \in rootSet <=> val \in S
     e = false */
  while(n != null && !e) {
    /* Loop invariant: rbtree(n, S, ..)  and
       val \in rootSet <=> val \in S
     */
    nodeVal := n->value;

    if(val == nodeVal) {
      /* val \in {v}
         |= val \in rootSet
       */
      e = true;
    }
    else {
      /* val \notin {v} */

      if(val < nodeVal) {
        /* val notin {v}
           val notin R
           {v} u Q u R = S
           Q = S - {v} - R
           val maybe in Q */
        n = n->left;
	/* rbtree(n, Q, ..)  and
           val \in Q  ==>  val \in rootSet
	 */
      }
      else {
        /* val notin {v}; val not in Q; val maybe in R */
        n = n->right;
      }

      /* val \in S ==> val \in rootSet
       */
    }

    
  }

  /* EITHER we exited the loop because e is true, OR because n is null, but not both.
     because e is true ==> val \in S
     because n is null ==> val \notin S
   */
}
