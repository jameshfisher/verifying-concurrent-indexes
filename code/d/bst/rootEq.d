bool rootEq(Node* root, int value) {
  // Function precondition.
  /// <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
  
  // Open <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/></e:fst>
  ///   <e:snd><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:snd>
  /// </e:exists>

  // Open <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
  ///   <e:snd><e:indent>
  ///     <e:sep>
  ///       <e:fst><e:fcell>
  ///	      <e:fst><code>root</code></e:fst>
  ///	      <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
  ///	    </e:fcell></e:fst>
  ///       <e:snd><e:sep>
  ///	      <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
  ///	      <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
  ///       </e:sep></e:snd>
  ///     </e:sep> ∧<br />
  ///     <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///   </e:indent></e:snd>
  /// </e:exists>

  bool o = root.value == value;
  // Assignment.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
  ///   <e:snd><e:indent>
  ///     <e:sep>
  ///       <e:fst><e:fcell>
  ///	      <e:fst><code>root</code></e:fst>
  ///	      <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
  ///	    </e:fcell></e:fst>
  ///       <e:snd><e:sep>
  ///	      <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
  ///	      <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
  ///       </e:sep></e:snd>
  ///     </e:sep> ∧<br />
  ///     <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
  ///     <e:doubleimpl>
  ///       <e:fst><code>o</code></e:fst>
  ///       <e:snd><e:eq>
  ///         <e:fst><e:var n="v"/></e:fst>
  ///         <e:snd><code>value</code></e:snd>
  ///       </e:eq></e:snd>
  ///     </e:doubleimpl>
  ///   </e:indent></e:snd>
  /// </e:exists>

  // Close <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>. Postcondition.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/></e:fst>
  ///   <e:snd><e:and>
  ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
  ///     <e:snd><e:doubleimpl>
  ///       <e:fst><code>o</code></e:fst>
  ///       <e:snd><e:eq>
  ///         <e:fst><e:var n="v"/></e:fst>
  ///         <e:snd><code>value</code></e:snd>
  ///       </e:eq></e:snd>
  ///     </e:doubleimpl></e:snd>
  ///   </e:and></e:snd>
  /// </e:exists>
  return o;
}