module bst.descend;

import bst.node;


Node* descend(Node* root, in int value) {
  // Function precondition.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/></e:fst>
  ///   <e:snd><e:and>
  ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
  ///     <e:snd><e:noteq>
  ///         <e:fst><e:var n="v"/></e:fst>
  ///         <e:snd><code>value</code></e:snd>
  ///       </e:noteq></e:snd>
  ///   </e:and></e:snd>
  /// </e:exists>

  // <e:logimpl>
  //   <e:fst><e:noteq>
  //     <e:fst><e:var n="a"/></e:fst>
  //     <e:snd><e:var n="b"/></e:snd>
  //   </e:noteq></e:fst>
  //   <e:snd><e:or>
  //     <e:fst><e:lt>
  //       <e:fst><e:var n="a"/></e:fst>
  //       <e:snd><e:var n="b"/></e:snd>
  //     </e:lt></e:fst>
  //     <e:snd><e:lt>
  //       <e:fst><e:var n="b"/></e:fst>
  //       <e:snd><e:var n="a"/></e:snd>
  //     </e:lt></e:snd>
  //   </e:or></e:snd>
  // </e:logimpl>
  /// <e:exists>
  ///   <e:fst><e:var n="v"/></e:fst>
  ///   <e:snd><e:and>
  ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
  ///     <e:snd><e:or>
  ///       <e:fst>(<e:lt>
  ///         <e:fst><e:var n="v"/></e:fst>
  ///         <e:snd><code>value</code></e:snd>
  ///       </e:lt>)</e:fst>
  ///       <e:snd>(<e:lt>
  ///         <e:fst><code>value</code></e:fst>
  ///         <e:snd><e:var n="v"/></e:snd>
  ///       </e:lt>)</e:snd>
  ///     </e:or></e:snd>
  ///   </e:and></e:snd>
  /// </e:exists>

  int dir = value > root.value;

  // C semantics render this as an integer.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/></e:fst>
  ///   <e:snd><e:and>
  ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
  ///     <e:snd><br /><e:indent><e:or>
  ///       <e:fst>(<e:and>
  ///         <e:fst><e:lt>
  ///           <e:fst><e:var n="v"/></e:fst>
  ///           <e:snd><code>value</code></e:snd>
  ///         </e:lt></e:fst>
  ///         <e:snd><e:eq>
  ///           <e:fst><code>dir</code></e:fst>
  ///           <e:snd><e:const n="0"/></e:snd>
  ///         </e:eq></e:snd>
  ///       </e:and>)</e:fst>
  ///       <e:snd><br />(<e:and>
  ///         <e:fst><e:lt>
  ///           <e:fst><code>value</code></e:fst>
  ///           <e:snd><e:var n="v"/></e:snd>
  ///         </e:lt></e:fst>
  ///         <e:snd><e:eq>
  ///           <e:fst><code>dir</code></e:fst>
  ///           <e:snd><e:const n="1"/></e:snd>
  ///         </e:eq></e:snd>
  ///       </e:and>)</e:snd>
  ///     </e:or></e:indent></e:snd>
  ///   </e:and></e:snd>
  /// </e:exists>

  // Open <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
  ///   <e:snd><e:indent><e:and>
  ///     <e:fst>
  ///       <e:sep>
  ///         <e:fst><e:fcell>
  ///	        <e:fst><code>root</code></e:fst>
  ///	        <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
  ///	      </e:fcell></e:fst>
  ///         <e:snd><e:sep>
  ///	        <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
  ///	        <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
  ///         </e:sep></e:snd>
  ///       </e:sep> ∧<br />
  ///       <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///     </e:fst>
  ///     <e:snd><br /><e:indent><e:or>
  ///       <e:fst>(<e:and>
  ///         <e:fst><e:lt>
  ///           <e:fst><e:var n="v"/></e:fst>
  ///           <e:snd><code>value</code></e:snd>
  ///         </e:lt></e:fst>
  ///         <e:snd><e:eq>
  ///           <e:fst><code>dir</code></e:fst>
  ///           <e:snd><e:const n="0"/></e:snd>
  ///         </e:eq></e:snd>
  ///       </e:and>)</e:fst>
  ///       <e:snd><br />(<e:and>
  ///         <e:fst><e:lt>
  ///           <e:fst><code>value</code></e:fst>
  ///           <e:snd><e:var n="v"/></e:snd>
  ///         </e:lt></e:fst>
  ///         <e:snd><e:eq>
  ///           <e:fst><code>dir</code></e:fst>
  ///           <e:snd><e:const n="1"/></e:snd>
  ///         </e:eq></e:snd>
  ///       </e:and>)</e:snd>
  ///     </e:or></e:indent></e:snd>
  ///   </e:and></e:indent></e:snd>
  /// </e:exists>

  Node* o = root.c[dir];
  // Both cases of <code>dir</code>.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
  ///   <e:snd><e:indent><e:and>
  ///     <e:fst>
  ///       <e:sep>
  ///         <e:fst><e:fcell>
  ///	        <e:fst><code>root</code></e:fst>
  ///	        <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
  ///	      </e:fcell></e:fst>
  ///         <e:snd><e:sep>
  ///	        <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
  ///	        <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
  ///         </e:sep></e:snd>
  ///       </e:sep> ∧<br />
  ///       <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///     </e:fst>
  ///     <e:snd><br /><e:indent><e:or>
  ///       <e:fst>(<e:and>
  ///         <e:fst><e:lt>
  ///           <e:fst><e:var n="v"/></e:fst>
  ///           <e:snd><code>value</code></e:snd>
  ///         </e:lt></e:fst>
  ///         <e:snd><e:eq>
  ///           <e:fst><code>o</code></e:fst>
  ///           <e:snd><e:var n="l"/></e:snd>
  ///         </e:eq></e:snd>
  ///       </e:and>)</e:fst>
  ///       <e:snd><br />(<e:and>
  ///         <e:fst><e:lt>
  ///           <e:fst><code>value</code></e:fst>
  ///           <e:snd><e:var n="v"/></e:snd>
  ///         </e:lt></e:fst>
  ///         <e:snd><e:eq>
  ///           <e:fst><code>o</code></e:fst>
  ///           <e:snd><e:var n="r"/></e:snd>
  ///         </e:eq></e:snd>
  ///       </e:and>)</e:snd>
  ///     </e:or></e:indent></e:snd>
  ///   </e:and></e:indent></e:snd>
  /// </e:exists>

  // Substitution.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
  ///   <e:snd><e:indent><e:and>
  ///     <e:fst>
  ///       <e:sep>
  ///         <e:fst><e:fcell>
  ///	        <e:fst><code>root</code></e:fst>
  ///	        <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
  ///	      </e:fcell></e:fst>
  ///         <e:snd><e:sep>
  ///	        <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
  ///	        <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
  ///         </e:sep></e:snd>
  ///       </e:sep> ∧<br />
  ///       <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///     </e:fst>
  ///     <e:snd><br /><e:indent><e:or>
  ///       <e:fst>(<e:and>
  ///         <e:fst><e:lt>
  ///           <e:fst><e:var n="v"/></e:fst>
  ///           <e:snd><code>value</code></e:snd>
  ///         </e:lt></e:fst>
  ///         <e:snd><e:pred name="Tree"><code>o</code>, <e:st n="L"/></e:pred></e:snd>
  ///       </e:and>)</e:fst>
  ///       <e:snd><br />(<e:and>
  ///         <e:fst><e:lt>
  ///           <e:fst><code>value</code></e:fst>
  ///           <e:snd><e:var n="v"/></e:snd>
  ///         </e:lt></e:fst>
  ///         <e:snd><e:pred name="Tree"><code>o</code>, <e:st n="R"/></e:pred></e:snd>
  ///       </e:and>)</e:snd>
  ///     </e:or></e:indent></e:snd>
  ///   </e:and></e:indent></e:snd>
  /// </e:exists>

  // Lemma: <e:logimpl>
  //   <e:fst><e:and>
  //     <e:fst><e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:fst>
  //     <e:snd><e:lt><e:fst><e:var n="v"/></e:fst><e:snd><code>value</code></e:snd></e:lt></e:snd>
  //   </e:and></e:fst>
  //   <e:snd><e:doubleimpl>
  //     <e:fst><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="R"/></e:snd></e:in></e:fst>
  //     <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
  //   </e:doubleimpl></e:snd>
  // </e:logimpl>.<br />
  // Lemma: <e:logimpl>
  //   <e:fst><e:and>
  //     <e:fst><e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:fst>
  //     <e:snd><e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt></e:snd>
  //   </e:and></e:fst>
  //   <e:snd><e:doubleimpl>
  //     <e:fst><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="L"/></e:snd></e:in></e:fst>
  //     <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
  //   </e:doubleimpl></e:snd>
  // </e:logimpl>.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
  ///   <e:snd><e:indent><e:and>
  ///     <e:fst>
  ///       <e:sep>
  ///         <e:fst><e:fcell>
  ///	        <e:fst><code>root</code></e:fst>
  ///	        <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
  ///	      </e:fcell></e:fst>
  ///         <e:snd><e:sep>
  ///	        <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
  ///	        <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
  ///         </e:sep></e:snd>
  ///       </e:sep> ∧<br />
  ///       <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///     </e:fst>
  ///     <e:snd><br /><e:indent><e:or>
  ///       <e:fst>(<e:and>
  ///         <e:fst><e:pred name="Tree"><code>o</code>, <e:st n="L"/></e:pred></e:fst>
  ///         <e:snd>(<e:doubleimpl>
  ///           <e:fst><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="L"/></e:snd></e:in></e:fst>
  ///           <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
  ///         </e:doubleimpl>)</e:snd>
  ///       </e:and>)</e:fst>
  ///       <e:snd><br />(<e:and>
  ///         <e:fst><e:pred name="Tree"><code>o</code>, <e:st n="R"/></e:pred></e:fst>
  ///         <e:snd>(<e:doubleimpl>
  ///           <e:fst><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="R"/></e:snd></e:in></e:fst>
  ///           <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
  ///         </e:doubleimpl>)</e:snd>
  ///       </e:and>)</e:snd>
  ///     </e:or></e:indent></e:snd>
  ///   </e:and></e:indent></e:snd>
  /// </e:exists>

  // <m:existsIntro/> on <e:st n="L"/> as <e:st n="Q"/>, <e:st n="R"/> as <e:st n="Q"/>.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
  ///   <e:snd><e:indent><e:and>
  ///     <e:fst>
  ///       <e:sep>
  ///         <e:fst><e:fcell>
  ///	        <e:fst><code>root</code></e:fst>
  ///	        <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
  ///	      </e:fcell></e:fst>
  ///         <e:snd><e:sep>
  ///	        <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
  ///	        <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
  ///         </e:sep></e:snd>
  ///       </e:sep> ∧<br />
  ///       <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///     </e:fst>
  ///     <e:snd><br /><e:indent><e:or>
  ///       <e:fst><e:exists><e:fst><e:st n="Q"/></e:fst><e:snd>(<e:and>
  ///         <e:fst><e:pred name="Tree"><code>o</code>, <e:st n="Q"/></e:pred></e:fst>
  ///         <e:snd>(<e:doubleimpl>
  ///           <e:fst><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="Q"/></e:snd></e:in></e:fst>
  ///           <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
  ///         </e:doubleimpl>)</e:snd>
  ///       </e:and>)</e:snd></e:exists></e:fst>
  ///       <e:snd><br /><e:exists><e:fst><e:st n="Q"/></e:fst><e:snd>(<e:and>
  ///         <e:fst><e:pred name="Tree"><code>o</code>, <e:st n="Q"/></e:pred></e:fst>
  ///         <e:snd>(<e:doubleimpl>
  ///           <e:fst><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="Q"/></e:snd></e:in></e:fst>
  ///           <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
  ///         </e:doubleimpl>)</e:snd>
  ///       </e:and>)</e:snd></e:exists></e:snd>
  ///     </e:or></e:indent></e:snd>
  ///   </e:and></e:indent></e:snd>
  /// </e:exists>

  // Introduce quantification.
  /// ∃v,l,r,L,R.
  ///   root↦v,l,r ∗ Tree(l, L) ∗ Tree(r, R)
  ///   ∧ Compose(L, v, R, S)
  ///   ∧ (∃Q. Tree(o, Q) ∧ (value∈Q ↔ value∈S)) ∨ (∃Q. Tree(o, Q) ∧ (value∈Q ↔ value∈S))

  // a ∨ a implies a.
  /// ∃v,l,r,L,R.
  ///   root↦v,l,r ∗ Tree(l, L) ∗ Tree(r, R)
  ///   ∧ Compose(L, v, R, S)
  ///   ∧ ∃Q. Tree(o, Q) ∧ (value∈Q ↔ value∈S))

  // Function postcondition.  Close TopOfTree.
  /// TopOfTree(root, v, S)
  /// ∧ ∃Q. Tree(o, Q) ∧ (value∈Q ↔ value∈S))

  // Note: we want |Q| &lt; |S|
  // to show termination of other functions.

  return o;
}
