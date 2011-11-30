bool rootEq(Node* root, int value) {
  // Function precondition.
  /// <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
  
  // Open <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/></e:vars>
  ///   <e:expr><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:expr>
  /// </e:exists>

  // Open <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
  ///   <e:expr><e:indent>
  ///     <e:sep>
  ///       <e:fcell>
  ///	      <code>root</code>
  ///	      <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
  ///	    </e:fcell>
  ///       <e:sep>
  ///	      <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
  ///	      <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
  ///       </e:sep>
  ///     </e:sep> ∧<br />
  ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///   </e:indent></e:expr>
  /// </e:exists>

  bool o = root.value == value;
  // Assignment.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
  ///   <e:expr><e:indent>
  ///     <e:sep>
  ///       <e:fcell>
  ///	      <code>root</code>
  ///	      <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
  ///	    </e:fcell>
  ///       <e:sep>
  ///	      <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
  ///	      <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
  ///       </e:sep>
  ///     </e:sep> ∧<br />
  ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
  ///     <e:doubleimpl>
  ///       <code>o</code>
  ///       <e:eq>
  ///         <e:var n="v"/>
  ///         <code>value</code>
  ///       </e:eq>
  ///     </e:doubleimpl>
  ///   </e:indent></e:expr>
  /// </e:exists>

  // Close <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>. Postcondition.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/></e:vars>
  ///   <e:expr><e:and>
  ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
  ///     <e:doubleimpl>
  ///       <code>o</code>
  ///       <e:eq>
  ///         <e:var n="v"/>
  ///         <code>value</code>
  ///       </e:eq>
  ///     </e:doubleimpl>
  ///   </e:and></e:expr>
  /// </e:exists>
  return o;
}