module ll.search.iterative;

import ll.node;

bool search(Node* head, int value) {
  /// <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>

  Node* i = head;

  while (i != null && i.value < value) {
    /// <e:exists>
    ///   <e:vars><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:var n="tail"/></e:vars>
    ///   <e:expr><e:indent><e:and>
    ///     <e:sep>
    ///       <e:pred name="ListSegment"><code>head</code>, <code>i</code>, <e:st n="L"/></e:pred>
    ///       <e:fcell><code>i</code>
    ///         <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
    ///       </e:fcell>
    ///       <e:pred name="List"><e:var n="tail"/>, <e:st n="R"/></e:pred>
    ///     </e:sep>
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:and></e:indent></e:expr>
    /// </e:exists>

    i = i.tail;
    /// <e:exists>
    ///   <e:vars><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:var n="tail"/></e:vars>
    ///   <e:expr><e:indent><e:and type="lines">
    ///     <e:sep type="lines">
    ///       <e:pred name="ListSegment"><code>head</code>, <e:var n="tail"/>, <e:st n="L"/></e:pred>
    ///       <e:fcell><code>i</code>
    ///         <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
    ///       </e:fcell>
    ///       <e:pred name="List"><e:var n="tail"/>, <e:st n="R"/></e:pred>
    ///     </e:sep>
    ///     <e:pred name="Cmp"><e:st n="L"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:and></e:indent></e:expr>
    /// </e:exists>
  }
  /// <e:exists>
  ///   <e:vars><e:st n="L"/><e:st n="R"/></e:vars>
  ///   <e:expr><e:indent><e:and type="lines">
  ///     <e:sep>
  ///       <e:pred name="ListSegment"><code>head</code>, <code>i</code>, <e:st n="L"/></e:pred>
  ///       <e:pred name="List"><code>i</code>, <e:st n="R"/></e:pred>
  ///     </e:sep>
  ///     <e:pred name="SegCompose"><e:st n="L"/>,<e:st n="R"/>,e:st n="S"/></e:pred>
  ///     <e:or parens="yes">
  ///       <e:eq><code>i</code><code>null</code></e:eq>
  ///       <e:leq><code>value</code><code>i.value</code></e:leq>
  ///     </e:or>
  ///   </e:and></e:indent></e:expr>
  /// </e:exists>

  bool o;
  if (i == null) {
    // Assert if-condition.
    /// <e:eq>
    ///   <code>i</code>
    ///   <code>null</code>
    /// </e:eq>

    /// <e:and>
    ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    ///   <e:notin>
    ///     <code>value</code>
    ///     <e:st n="S"/>
    ///   </e:notin>
    /// </e:and>

    o = false;
    // If-rule.
    /// <e:and>
    ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    ///   <e:doubleimpl>
    ///     <code>o</code>
    ///     <e:in>
    ///       <code>value</code>
    ///       <e:st n="S"/>
    ///     </e:in>
    ///   </e:doubleimpl>
    /// </e:and>
  }
  else {
    // Deny if-condition.
    /// <e:noteq>
    ///   <code>i</code>
    ///   <code>null</code>
    /// </e:noteq>

    if (i.value == value) {
      /// <e:and>
      ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
      ///   <e:in>
      ///     <code>value</code>
      ///     <e:st n="S"/>
      ///   </e:in>
      /// </e:and>
      o = true;
      /// <e:and>
      ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
      ///   <e:doubleimpl>
      ///     <code>o</code>
      ///     <e:in>
      ///       <code>value</code>
      ///       <e:st n="S"/>
      ///     </e:in>
      ///   </e:doubleimpl>
      /// </e:and>
    }
    else {
      /// <e:and>
      ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
      ///   <e:notin>
      ///     <code>value</code>
      ///     <e:st n="S"/>
      ///   </e:notin>
      /// </e:and>
      o = false;
      // Assignment. ??
      /// <e:and>
      ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
      ///   <e:doubleimpl>
      ///     <code>o</code>
      ///     <e:in>
      ///       <code>value</code>
      ///       <e:st n="S"/>
      ///     </e:in>
      ///   </e:doubleimpl>
      /// </e:and>
    }
    // If-rule.
    /// <e:and>
    ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    ///   <e:doubleimpl>
    ///     <code>o</code>
    ///     <e:in>
    ///       <code>value</code>
    ///       <e:st n="S"/>
    ///     </e:in>
    ///   </e:doubleimpl>
    /// </e:and>
  }
  // If-rule.
  /// <e:and>
  ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
  ///   <e:doubleimpl>
  ///     <code>o</code>
  ///     <e:in>
  ///       <code>value</code>
  ///       <e:st n="S"/>
  ///     </e:in>
  ///   </e:doubleimpl>
  /// </e:and>

  return o;
}