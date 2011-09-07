module rb.blacken;

import rb.node;

Node* blacken(Node* root) {
  // Function precondition.
  /// <e:pred name="RT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>

  // Open <e:pred name="RT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
  ///   <e:snd><e:indent><e:and>
  ///     <e:fst><e:sep>
  ///       <e:fst><e:fcell>
  ///         <e:fst><code>root</code></e:fst>
  ///         <e:snd><e:var n="v"/>,<m:red/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
  ///       </e:fcell></e:fst>
  ///       <e:snd><e:sep>
  ///         <e:fst><e:pred name="BT"><e:var n="l"/>,<e:st n="L"/>,<e:var n="h"/></e:pred></e:fst>
  ///         <e:snd><e:pred name="BT"><e:var n="r"/>,<e:st n="R"/>,<e:var n="h"/></e:pred></e:snd>
  ///       </e:sep></e:snd>
  ///     </e:sep></e:fst>
  ///     <e:snd><br /><e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:snd>
  ///   </e:and></e:indent></e:snd>
  /// </e:exists>

  // <e:logimpl>
  //   <e:fst><e:fcell><e:fst><e:var n="a"/></e:fst><e:snd>_</e:snd></e:fcell></e:fst>
  //   <e:snd><e:noteq><e:fst><e:var n="a"/></e:fst><e:snd><code>null</code></e:snd></e:noteq></e:snd>
  // </e:logimpl>
  /// <e:exists>
  ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
  ///   <e:snd><e:indent><e:and>
  ///     <e:fst><e:sep>
  ///       <e:fst><e:fcell>
  ///         <e:fst><code>root</code></e:fst>
  ///         <e:snd><e:var n="v"/>,<m:red/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
  ///       </e:fcell></e:fst>
  ///       <e:snd><e:sep>
  ///         <e:fst><e:pred name="BT"><e:var n="l"/>,<e:st n="L"/>,<e:var n="h"/></e:pred></e:fst>
  ///         <e:snd><e:pred name="BT"><e:var n="r"/>,<e:st n="R"/>,<e:var n="h"/></e:pred></e:snd>
  ///       </e:sep></e:snd>
  ///     </e:sep></e:fst>
  ///     <e:snd><br/><e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:snd> ∧
  ///     <e:noteq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:noteq>
  ///   </e:and></e:indent></e:snd>
  /// </e:exists>

  if (root == null) {
    // Assert if-condition.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent><e:and>
    ///     <e:fst><e:sep>
    ///       <e:fst><e:fcell>
    ///         <e:fst><code>root</code></e:fst>
    ///         <e:snd><e:var n="v"/>,<m:red/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
    ///       </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///         <e:fst><e:pred name="BT"><e:var n="l"/>,<e:st n="L"/>,<e:var n="h"/></e:pred></e:fst>
    ///         <e:snd><e:pred name="BT"><e:var n="r"/>,<e:st n="R"/>,<e:var n="h"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep></e:fst>
    ///     <e:snd><br/><e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:snd> ∧
    ///     <e:noteq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:noteq> ∧
    ///     <e:eq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:eq>
    ///   </e:and></e:indent></e:snd>
    /// </e:exists>

    // Contradiction: this branch does not execute.
    /// <e:noteq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:noteq> ∧
    /// <e:eq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:eq>
  }
  else {
    // Deny if-condition.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent><e:and>
    ///     <e:fst><e:sep>
    ///       <e:fst><e:fcell>
    ///         <e:fst><code>root</code></e:fst>
    ///         <e:snd><e:var n="v"/>,<m:red/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
    ///       </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///         <e:fst><e:pred name="BT"><e:var n="l"/>,<e:st n="L"/>,<e:var n="h"/></e:pred></e:fst>
    ///         <e:snd><e:pred name="BT"><e:var n="r"/>,<e:st n="R"/>,<e:var n="h"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep></e:fst>
    ///     <e:snd><br/><e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:snd> ∧
    ///     <e:noteq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:noteq> ∧
    ///     <e:noteq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:noteq>
    ///   </e:and></e:indent></e:snd>
    /// </e:exists>

    // Discard <e:noteq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:noteq> (twice).
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent><e:and>
    ///     <e:fst><e:sep>
    ///       <e:fst><e:fcell>
    ///         <e:fst><code>root</code></e:fst>
    ///         <e:snd><e:var n="v"/>,<m:red/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
    ///       </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///         <e:fst><e:pred name="BT"><e:var n="l"/>,<e:st n="L"/>,<e:var n="h"/></e:pred></e:fst>
    ///         <e:snd><e:pred name="BT"><e:var n="r"/>,<e:st n="R"/>,<e:var n="h"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep></e:fst>
    ///     <e:snd><br/><e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:snd>
    ///   </e:and></e:indent></e:snd>
    /// </e:exists>

    // <m:existsIntro/>
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/>, <e:var n="c"/></e:fst>
    ///   <e:snd><e:indent><e:and>
    ///     <e:fst><e:sep>
    ///       <e:fst><e:fcell>
    ///         <e:fst><code>root</code></e:fst>
    ///         <e:snd><e:var n="v"/>,<e:var n="c"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
    ///       </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///         <e:fst><e:pred name="BT"><e:var n="l"/>,<e:st n="L"/>,<e:var n="h"/></e:pred></e:fst>
    ///         <e:snd><e:pred name="BT"><e:var n="r"/>,<e:st n="R"/>,<e:var n="h"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep></e:fst>
    ///     <e:snd><br/><e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:snd> ∧
    ///     <e:eq><e:fst><e:var n="c"/></e:fst><e:snd><m:red/></e:snd></e:eq>
    ///   </e:and></e:indent></e:snd>
    /// </e:exists>

    if (root.black) {
      // Assert if-condition.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/>, <e:var n="c"/></e:fst>
      ///   <e:snd><e:indent><e:and>
      ///     <e:fst><e:sep>
      ///       <e:fst><e:fcell>
      ///         <e:fst><code>root</code></e:fst>
      ///         <e:snd><e:var n="v"/>,<e:var n="c"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
      ///       </e:fcell></e:fst>
      ///       <e:snd><e:sep>
      ///         <e:fst><e:pred name="BT"><e:var n="l"/>,<e:st n="L"/>,<e:var n="h"/></e:pred></e:fst>
      ///         <e:snd><e:pred name="BT"><e:var n="r"/>,<e:st n="R"/>,<e:var n="h"/></e:pred></e:snd>
      ///       </e:sep></e:snd>
      ///     </e:sep></e:fst>
      ///     <e:snd><br/><e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:snd> ∧
      ///     <e:eq><e:fst><e:var n="c"/></e:fst><e:snd><m:red/></e:snd></e:eq> ∧
      ///     <e:eq><e:fst><e:var n="c"/></e:fst><e:snd><m:black/></e:snd></e:eq>
      ///   </e:and></e:indent></e:snd>
      /// </e:exists>

      // Contradiction: this branch does not execute.
      /// <e:eq><e:fst><e:var n="c"/></e:fst><e:snd><m:red/></e:snd></e:eq> ∧
      /// <e:eq><e:fst><e:var n="c"/></e:fst><e:snd><m:black/></e:snd></e:eq>
    }
    else {
      // Deny if-condition.  Substitution.  Discard <e:eq><e:fst><e:var n="c"/></e:fst><e:snd><m:red/></e:snd></e:eq> (twice).
      /// <e:exists>
      ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
      ///   <e:snd><e:indent><e:and>
      ///     <e:fst><e:sep>
      ///       <e:fst><e:fcell>
      ///         <e:fst><code>root</code></e:fst>
      ///         <e:snd><e:var n="v"/>,<m:red/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
      ///       </e:fcell></e:fst>
      ///       <e:snd><e:sep>
      ///         <e:fst><e:pred name="BT"><e:var n="l"/>,<e:st n="L"/>,<e:var n="h"/></e:pred></e:fst>
      ///         <e:snd><e:pred name="BT"><e:var n="r"/>,<e:st n="R"/>,<e:var n="h"/></e:pred></e:snd>
      ///       </e:sep></e:snd>
      ///     </e:sep></e:fst>
      ///     <e:snd><br/><e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:snd>
      ///   </e:and></e:indent></e:snd>
      /// </e:exists>
      
      root.black = true;
      /// <e:exists>
      ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
      ///   <e:snd><e:indent><e:and>
      ///     <e:fst><e:sep>
      ///       <e:fst><e:fcell>
      ///         <e:fst><code>root</code></e:fst>
      ///         <e:snd><e:var n="v"/>,<m:black/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
      ///       </e:fcell></e:fst>
      ///       <e:snd><e:sep>
      ///         <e:fst><e:pred name="BT"><e:var n="l"/>,<e:st n="L"/>,<e:var n="h"/></e:pred></e:fst>
      ///         <e:snd><e:pred name="BT"><e:var n="r"/>,<e:st n="R"/>,<e:var n="h"/></e:pred></e:snd>
      ///       </e:sep></e:snd>
      ///     </e:sep></e:fst>
      ///     <e:snd><br/><e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:snd>
      ///   </e:and></e:indent></e:snd>
      /// </e:exists>

      // Close <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus></e:pred>.
      /// <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus></e:pred>
    }
    // Postcondition of executing branch.
    /// <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus></e:pred>
  }
  // Postcondition of executing branch.
  /// <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus></e:pred>

  return root;
}