module bst.descend;

import bst.node;


Node* descend(Node* root, in int value) {
  // Function precondition.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/></e:vars>
  ///   <e:expr><e:and>
  ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
  ///     <e:noteq>
  ///         <e:var n="v"/>
  ///         <code>value</code>
  ///       </e:noteq>
  ///   </e:and></e:expr>
  /// </e:exists>

  // <e:logimpl>
  //   <e:noteq>
  //     <e:var n="a"/>
  //     <e:var n="b"/>
  //   </e:noteq>
  //   <e:or>
  //     <e:lt>
  //       <e:var n="a"/>
  //       <e:var n="b"/>
  //     </e:lt>
  //     <e:lt>
  //       <e:var n="b"/>
  //       <e:var n="a"/>
  //     </e:lt>
  //   </e:or>
  // </e:logimpl>
  /// <e:exists>
  ///   <e:vars><e:var n="v"/></e:vars>
  ///   <e:expr><e:and>
  ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
  ///     <e:or>
  ///       (<e:lt>
  ///         <e:var n="v"/>
  ///         <code>value</code>
  ///       </e:lt>)
  ///       (<e:lt>
  ///         <code>value</code>
  ///         <e:var n="v"/>
  ///       </e:lt>)
  ///     </e:or>
  ///   </e:and></e:expr>
  /// </e:exists>

  int dir = value > root.value;

  // C semantics render this as an integer.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/></e:vars>
  ///   <e:expr><e:and>
  ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
  ///     <br /><e:indent><e:or>
  ///       (<e:and>
  ///         <e:lt>
  ///           <e:var n="v"/>
  ///           <code>value</code>
  ///         </e:lt>
  ///         <e:eq>
  ///           <code>dir</code>
  ///           <e:const n="0"/>
  ///         </e:eq>
  ///       </e:and>)
  ///       <br />(<e:and>
  ///         <e:lt>
  ///           <code>value</code>
  ///           <e:var n="v"/>
  ///         </e:lt>
  ///         <e:eq>
  ///           <code>dir</code>
  ///           <e:const n="1"/>
  ///         </e:eq>
  ///       </e:and>)
  ///     </e:or></e:indent>
  ///   </e:and></e:expr>
  /// </e:exists>

  // Open <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
  ///   <e:expr><e:indent><e:and>
  ///     
  ///       <e:sep>
  ///         <e:fcell>
  ///	        <code>root</code>
  ///	        <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
  ///	      </e:fcell>
  ///         <e:sep>
  ///	        <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
  ///	        <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
  ///         </e:sep>
  ///       </e:sep> ∧<br />
  ///       <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///     
  ///     <br /><e:indent><e:or>
  ///       (<e:and>
  ///         <e:lt>
  ///           <e:var n="v"/>
  ///           <code>value</code>
  ///         </e:lt>
  ///         <e:eq>
  ///           <code>dir</code>
  ///           <e:const n="0"/>
  ///         </e:eq>
  ///       </e:and>)
  ///       <br />(<e:and>
  ///         <e:lt>
  ///           <code>value</code>
  ///           <e:var n="v"/>
  ///         </e:lt>
  ///         <e:eq>
  ///           <code>dir</code>
  ///           <e:const n="1"/>
  ///         </e:eq>
  ///       </e:and>)
  ///     </e:or></e:indent>
  ///   </e:and></e:indent></e:expr>
  /// </e:exists>

  Node* o = root.c[dir];
  // Both cases of <code>dir</code>.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
  ///   <e:expr><e:indent><e:and>
  ///     
  ///       <e:sep>
  ///         <e:fcell>
  ///	        <code>root</code>
  ///	        <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
  ///	      </e:fcell>
  ///         <e:sep>
  ///	        <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
  ///	        <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
  ///         </e:sep>
  ///       </e:sep> ∧<br />
  ///       <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///     
  ///     <br /><e:indent><e:or>
  ///       (<e:and>
  ///         <e:lt>
  ///           <e:var n="v"/>
  ///           <code>value</code>
  ///         </e:lt>
  ///         <e:eq>
  ///           <code>o</code>
  ///           <e:var n="l"/>
  ///         </e:eq>
  ///       </e:and>)
  ///       <br />(<e:and>
  ///         <e:lt>
  ///           <code>value</code>
  ///           <e:var n="v"/>
  ///         </e:lt>
  ///         <e:eq>
  ///           <code>o</code>
  ///           <e:var n="r"/>
  ///         </e:eq>
  ///       </e:and>)
  ///     </e:or></e:indent>
  ///   </e:and></e:indent></e:expr>
  /// </e:exists>

  // Substitution.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
  ///   <e:expr><e:indent><e:and>
  ///     
  ///       <e:sep>
  ///         <e:fcell>
  ///	        <code>root</code>
  ///	        <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
  ///	      </e:fcell>
  ///         <e:sep>
  ///	        <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
  ///	        <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
  ///         </e:sep>
  ///       </e:sep> ∧<br />
  ///       <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///     
  ///     <br /><e:indent><e:or>
  ///       (<e:and>
  ///         <e:lt>
  ///           <e:var n="v"/>
  ///           <code>value</code>
  ///         </e:lt>
  ///         <e:pred name="Tree"><code>o</code>, <e:st n="L"/></e:pred>
  ///       </e:and>)
  ///       <br />(<e:and>
  ///         <e:lt>
  ///           <code>value</code>
  ///           <e:var n="v"/>
  ///         </e:lt>
  ///         <e:pred name="Tree"><code>o</code>, <e:st n="R"/></e:pred>
  ///       </e:and>)
  ///     </e:or></e:indent>
  ///   </e:and></e:indent></e:expr>
  /// </e:exists>

  // Lemma: <e:logimpl>
  //   <e:and>
  //     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  //     <e:lt><e:var n="v"/><code>value</code></e:lt>
  //   </e:and>
  //   <e:doubleimpl>
  //     <e:in><code>value</code><e:st n="R"/></e:in>
  //     <e:in><code>value</code><e:st n="S"/></e:in>
  //   </e:doubleimpl>
  // </e:logimpl>.<br />
  // Lemma: <e:logimpl>
  //   <e:and>
  //     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  //     <e:lt><code>value</code><e:var n="v"/></e:lt>
  //   </e:and>
  //   <e:doubleimpl>
  //     <e:in><code>value</code><e:st n="L"/></e:in>
  //     <e:in><code>value</code><e:st n="S"/></e:in>
  //   </e:doubleimpl>
  // </e:logimpl>.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
  ///   <e:expr><e:indent><e:and>
  ///     
  ///       <e:sep>
  ///         <e:fcell>
  ///	        <code>root</code>
  ///	        <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
  ///	      </e:fcell>
  ///         <e:sep>
  ///	        <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
  ///	        <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
  ///         </e:sep>
  ///       </e:sep> ∧<br />
  ///       <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///     
  ///     <br /><e:indent><e:or>
  ///       (<e:and>
  ///         <e:pred name="Tree"><code>o</code>, <e:st n="L"/></e:pred>
  ///         (<e:doubleimpl>
  ///           <e:in><code>value</code><e:st n="L"/></e:in>
  ///           <e:in><code>value</code><e:st n="S"/></e:in>
  ///         </e:doubleimpl>)
  ///       </e:and>)
  ///       <br />(<e:and>
  ///         <e:pred name="Tree"><code>o</code>, <e:st n="R"/></e:pred>
  ///         (<e:doubleimpl>
  ///           <e:in><code>value</code><e:st n="R"/></e:in>
  ///           <e:in><code>value</code><e:st n="S"/></e:in>
  ///         </e:doubleimpl>)
  ///       </e:and>)
  ///     </e:or></e:indent>
  ///   </e:and></e:indent></e:expr>
  /// </e:exists>

  // <m:existsIntro/> on <e:st n="L"/> as <e:st n="Q"/>, <e:st n="R"/> as <e:st n="Q"/>.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
  ///   <e:expr><e:indent><e:and>
  ///     
  ///       <e:sep>
  ///         <e:fcell>
  ///	        <code>root</code>
  ///	        <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
  ///	      </e:fcell>
  ///         <e:sep>
  ///	        <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
  ///	        <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
  ///         </e:sep>
  ///       </e:sep> ∧<br />
  ///       <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///     
  ///     <br /><e:indent><e:or>
  ///       <e:exists><e:st n="Q"/>(<e:and>
  ///         <e:pred name="Tree"><code>o</code>, <e:st n="Q"/></e:pred>
  ///         (<e:doubleimpl>
  ///           <e:in><code>value</code><e:st n="Q"/></e:in>
  ///           <e:in><code>value</code><e:st n="S"/></e:in>
  ///         </e:doubleimpl>)
  ///       </e:and>)</e:exists>
  ///       <br /><e:exists><e:st n="Q"/>(<e:and>
  ///         <e:pred name="Tree"><code>o</code>, <e:st n="Q"/></e:pred>
  ///         (<e:doubleimpl>
  ///           <e:in><code>value</code><e:st n="Q"/></e:in>
  ///           <e:in><code>value</code><e:st n="S"/></e:in>
  ///         </e:doubleimpl>)
  ///       </e:and>)</e:exists>
  ///     </e:or></e:indent>
  ///   </e:and></e:indent></e:expr>
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
