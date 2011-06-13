bool search(n, T val) {
  // $\Pred{rbtree}{\IC{n},{}S}.$
  //  $n=\nullref\Fand{}S=\scemp\Fand{}pH=1\Fand\hemp$
  // $\For$
  //  $\exists{}l,r,v,c,h.$
  //   $\Fcell{n}{l,r,v,c}\Fsep\Pred{tree}{l,Q,h,c}\Fsep\Pred{tree}{r,R,h,c}$
  //   $\Fand\forall{}v\prime.v\prime\in{}Q\IMPLIES{}v\prime{}<v\Fand\forall{}v\prime.v\prime\in{}R\IMPLIES{}v\prime{}>v$
  //   $\Fand{}Q\union{}R\union{}\Set{v}=S$
  //   $\Fand\Pred{rbCond}{c,pC,h,pH}.$
  if(n == nil) {
    // $n=\nullref\Fand{}S=\scemp\Fand{}pH=1\Fand\hemp\Fand{}val\notin{}S.$
    e = false;
    // $n=\nullref\Fand{}S=\scemp\Fand{}pH=1\Fand\hemp\Fand{}val\notin{}S\Fand{}e=\falsity.$
  }
  else {
    // $\exists{}l,r,v,c,h.$
    //  $\Fcell{n}{l,r,v,c}\Fsep\Pred{tree}{l,Q,h,c}\Fsep\Pred{tree}{r,R,h,c}$
    //  $\Fand\forall{}v\prime.v\prime\in{}Q\IMPLIES{}v\prime{}<v\Fand\forall{}v\prime.v\prime\in{}R\IMPLIES{}v\prime{}>v$
    //  $\Fand{}Q\union{}R\union{}\Set{v}=S$
    //  $\Fand\Pred{rbCond}{c,pC,h,pH}$
    rv := node->value;
    // $\ldots\Fand{}rv=v$
    if(val == rv) {
      // $\ldots\Fand{}rv=v\Fand{}val=rv\Fand{}val=v\Fand{}val\in{}S$
      e = true;
      // $\ldots{}val\in{}S\Fand{}e=\truth$
    }
    else {
      // $\ldots\Fand{}rv=v\Fand{}val\neq{}rv\Fand{}val\neq{}v$
      if(val < rv) {
        // $\ldots$
        // $\Fand\Pred{tree}{l,\Setb{v\prime}{v\prime\in{}S\Fand{}v\prime{}<v},h,c}$
        // $\Fand\Setb{v\prime}{v\prime\in{}S\Fand{}v\prime{}<v}\union\Setb{v\prime}{v\prime\in{}S\Fand{}v\prime{}>v}\union\Set{v}=S$
        // $\Fand{}rv=v\Fand{}val\neq{}v\Fand{}val<rv\Fand{}val<v\Fand{}v\notin\Setb{v\prime}{v\prime\in{}S\Fand{}v\prime>v}$
        // $\Fand{}v\notin\Set{v}\Fand{}truthvalueof(v\in{}S)=truthvalueof(v\in\Setb{v\prime}{v\prime\in{}S\Fand{}v\prime{}<v})$
        // $\Fand\mathsf{search}(r,val)=\mathsf{search}(l,val)$
        e = search(node->left, val);
        // $\ldots$
        // $\Fand{}rv=v\Fand{}val\neq{}v\Fand{}val<rv\Fand{}val<v$
        // $\Fand\Pred{tree}{l,\Setb{v\prime}{v\prime\in{}S\Fand{}v\prime{}<v},h,c}$
        // $\Fand\Pred{tree}{r,\Setb{v\prime}{v\prime\in{}S\Fand{}v\prime{}>v},h,c}$
        // $\Fand{}v\notin\Setb{v\prime}{v\prime\in{}S\Fand{}v\prime{}>v}$
        // $\Fand\mathsf{search}(r,val)=\mathsf{search}(l,val)$
      }
      else {
        e = search(node->right, val);
      }
    }
  }
  // $\Pred{rbtree}{node, S \union \Set{v}}$
}
