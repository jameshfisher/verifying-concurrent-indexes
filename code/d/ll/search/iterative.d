module ll.search.iterative;

import ll.node;

bool search(Node* head, int value) {
  /// <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>

  Node* i = head;

  // Loop invariant.
  /// <e:exists>
  ///   <e:vars><e:st n="L"/><e:st n="R"/></e:vars>
  ///   <e:expr><e:indent><e:and type="lines">
  ///     <e:sep>
  ///       <e:pred name="ListSegment"><code>head</code><code>i</code><e:st n="L"/></e:pred>
  ///       <e:pred name="List"><code>i</code><e:st n="R"/></e:pred>
  ///     </e:sep>
  ///     <e:pred name="SegmentCompose"><e:st n="L"/><e:st n="R"/><e:st n="S"/></e:pred>
  ///     <e:pred name="SetLT"><e:st n="L"/><code>value</code></e:pred>
  ///   </e:and></e:indent></e:expr>
  /// </e:exists>

  while (i != null && i.value < value) {
    // <e:logimpl>
    //   <e:noteq><code>i</code><code>null</code></e:noteq>
    //   <e:pred name="NonEmptyList"><code>i</code><e:st n="R"/></e:pred>
    // </e:logimpl>.
    // Open <e:pred name="NonEmptyList"><code>i</code><e:st n="R"/></e:pred>.
    // Loop condition.
    /// <e:exists>
    ///   <e:vars><e:st n="L"/><e:st n="R"/><e:var n="v"/><e:var n="tail"/><e:st n="T"/></e:vars>
    ///   <e:expr><e:indent><e:and type="lines">
    ///     <e:sep>
    ///       <e:pred name="ListSegment"><code>head</code><code>i</code><e:st n="L"/></e:pred>
    ///       <e:fcell>
    ///         <code>i</code>
    ///         <e:list><e:var n="v" /><e:var n="tail"/></e:list>
    ///       </e:fcell>
    ///       <e:pred name="List"><e:var n="tail"/><e:st n="T"/></e:pred>
    ///     </e:sep>
    ///     <e:pred name="ListCompose"><e:var n="v"/><e:st n="T"/><e:st n="R"/></e:pred>
    ///     <e:pred name="SegmentCompose"><e:st n="L"/><e:st n="R"/><e:st n="S"/></e:pred>
    ///     <e:pred name="SetLT"><e:st n="L"/><code>value</code></e:pred>
    ///     <e:lt><e:var n="v"/><code>value</code></e:lt>
    ///   </e:and></e:indent></e:expr>
    /// </e:exists>

    i = i.tail;
    // Extract tail pointer.  Quantify previous <code>i</code> as <e:var n="j"/>.
    /// <e:exists>
    ///   <e:vars><e:st n="L"/><e:st n="R"/><e:var n="v"/><e:st n="T"/><e:var n="j"/></e:vars>
    ///   <e:expr><e:indent><e:and type="lines">
    ///     <e:sep>
    ///       <e:pred name="ListSegment"><code>head</code><e:var n="j"/><e:st n="L"/></e:pred>
    ///       <e:fcell>
    ///         <e:var n="j"/>
    ///         <e:list><e:var n="v" /><code>i</code></e:list>
    ///       </e:fcell>
    ///       <e:pred name="List"><code>i</code><e:st n="T"/></e:pred>
    ///     </e:sep>
    ///     <e:pred name="ListCompose"><e:var n="v"/><e:st n="T"/><e:st n="R"/></e:pred>
    ///     <e:pred name="SegmentCompose"><e:st n="L"/><e:st n="R"/><e:st n="S"/></e:pred>
    ///     <e:pred name="SetLT"><e:st n="L"/><code>value</code></e:pred>
    ///     <e:lt><e:var n="v"/><code>value</code></e:lt>
    ///   </e:and></e:indent></e:expr>
    /// </e:exists>

    // <e:logimpl>
    //   <e:and>
    //     <e:pred name="ListCompose"><e:var n="v"/><e:st n="T"/><e:st n="R"/></e:pred>
    //     <e:pred name="SegmentCompose"><e:st n="L"/><e:st n="R"/><e:st n="S"/></e:pred>
    //   </e:and>
    //   <e:pred name="SetLT"><e:st n="L"/><e:var n="v"/></e:pred>
    // </e:logimpl>.<br />
    // <e:logimpl>
    //   <e:and>
    //     <e:sep>
    //       <e:pred name="ListSegment"><code>head</code><e:var n="j"/><e:st n="L"/></e:pred>
    //       <e:fcell>
    //         <e:var n="j"/>
    //         <e:list><e:var n="v"/><code>i</code></e:list>
    //       </e:fcell>
    //     </e:sep>
    //     <e:pred name="SetLT"><e:st n="L"/><e:var n="v"/></e:pred>
    //   </e:and>
    //   <e:pred name="NonEmptyListSegment"><code>head</code><code>i</code><e:union><e:st n="L"/><e:set><e:var n="v"/></e:set></e:union></e:pred>
    // </e:logimpl>.<br />
    // <e:logimpl>
    //   <e:and>
    //     <e:pred name="SetLT"><e:st n="L"/><code>value</code></e:pred>
    //     <e:lt><e:var n="v"/><code>value</code></e:lt>
    //   </e:and>
    //   <e:pred name="SetLT"><e:union><e:st n="L"/><e:set><e:var n="v"/></e:set></e:union><code>value</code></e:pred>
    // </e:logimpl>.<br />
    // <e:logimpl>
    //   <e:and>
    //     <e:pred name="ListCompose"><e:var n="v"/><e:st n="T"/><e:st n="R"/></e:pred>
    //     <e:pred name="SegmentCompose"><e:st n="L"/><e:st n="R"/><e:st n="S"/></e:pred>
    //   </e:and>
    //   <e:pred name="SegmentCompose"><e:union><e:st n="L"/><e:set><e:var n="v"/></e:set></e:union><e:st n="T"/><e:st n="S"/></e:pred>
    // </e:logimpl>.
    /// <e:exists>
    ///   <e:vars><e:st n="L"/><e:var n="v"/><e:st n="T"/></e:vars>
    ///   <e:expr><e:indent><e:and type="lines">
    ///     <e:sep>
    ///       <e:pred name="NonEmptyListSegment"><code>head</code><code>i</code><e:union><e:st n="L"/><e:set><e:var n="v"/></e:set></e:union></e:pred>
    ///       <e:pred name="List"><code>i</code><e:st n="T"/></e:pred>
    ///     </e:sep>
    ///     <e:pred name="SegmentCompose"><e:union><e:st n="L"/><e:set><e:var n="v"/></e:set></e:union><e:st n="T"/><e:st n="S"/></e:pred>
    ///     <e:pred name="SetLT"><e:union><e:st n="L"/><e:set><e:var n="v"/></e:set></e:union><code>value</code></e:pred>
    ///   </e:and></e:indent></e:expr>
    /// </e:exists>

    // Weaken <e:predicate>NonEmptyListSegment</e:predicate>.
    // Quantify <e:union><e:st n="L"/><e:set><e:var n="v"/></e:set></e:union> as <e:st n="L"/>.
    // Rename <e:st n="T"/> as <e:st n="R"/>.
    // Re-establishes loop invariant.
    /// <e:exists>
    ///   <e:vars><e:st n="L"/><e:st n="R"/></e:vars>
    ///   <e:expr><e:indent><e:and type="lines">
    ///     <e:sep>
    ///       <e:pred name="ListSegment"><code>head</code><code>i</code><e:st n="L"/></e:pred>
    ///       <e:pred name="List"><code>i</code><e:st n="R"/></e:pred>
    ///     </e:sep>
    ///     <e:pred name="SegmentCompose"><e:st n="L"/><e:st n="R"/><e:st n="S"/></e:pred>
    ///     <e:pred name="SetLT"><e:st n="L"/><code>value</code></e:pred>
    ///   </e:and></e:indent></e:expr>
    /// </e:exists>
  }
  // Loop invariant and negated loop condition.
  /// <e:exists>
  ///   <e:vars><e:st n="L"/><e:st n="R"/></e:vars>
  ///   <e:expr><e:indent><e:and type="lines">
  ///     <e:sep>
  ///       <e:pred name="ListSegment"><code>head</code><code>i</code><e:st n="L"/></e:pred>
  ///       <e:indent><e:or parens="yes" type="lines">
  ///         <e:pred name="EmptyList"><code>i</code><e:st n="R"/></e:pred>
  ///         <e:exists>
  ///           <e:vars><e:var n="v"/><e:var n="tail"/><e:st n="T"/></e:vars>
  ///           <e:expr><e:and>
  ///             <e:sep>
  ///               <e:fcell>
  ///                 <code>i</code>
  ///                 <e:list><e:var n="v" /><e:var n="tail"/></e:list>
  ///               </e:fcell>
  ///               <e:pred name="List"><e:var n="tail"/><e:st n="T"/></e:pred>
  ///             </e:sep>
  ///             <e:pred name="ListCompose"><e:var n="v"/><e:st n="T"/><e:st n="R"/></e:pred>
  ///           </e:and></e:expr>
  ///         </e:exists>
  ///       </e:or></e:indent>
  ///     </e:sep>
  ///     <e:pred name="SegmentCompose"><e:st n="L"/><e:st n="R"/><e:st n="S"/></e:pred>
  ///     <e:pred name="SetLT"><e:st n="L"/><code>value</code></e:pred>
  ///   </e:and></e:indent></e:expr>
  /// </e:exists>

  bool o;
  if (i == null) {
    // Assert if-condition.  Eliminate non-<code>null</code> side of disjunction.<br />
    /// <e:exists>
    ///   <e:vars><e:st n="L"/><e:st n="R"/></e:vars>
    ///   <e:expr><e:indent><e:and type="lines">
    ///     <e:sep>
    ///       <e:pred name="ListSegment"><code>head</code><code>null</code><e:st n="L"/></e:pred>
    ///       <e:pred name="EmptyList"><code>null</code><e:st n="R"/></e:pred>
    ///     </e:sep>
    ///     <e:pred name="SegmentCompose"><e:st n="L"/><e:st n="R"/><e:st n="S"/></e:pred>
    ///     <e:pred name="SetLT"><e:st n="L"/><code>value</code></e:pred>
    ///   </e:and></e:indent></e:expr>
    /// </e:exists>

    // <e:logimpl><e:pred name="EmptyList"><code>null</code><e:st n="R"/></e:pred><e:eq><e:st n="R"/><m:empty/></e:eq></e:logimpl>.
    /// <e:exists>
    ///   <e:vars><e:st n="L"/></e:vars>
    ///   <e:expr><e:indent><e:and type="lines">
    ///     <e:sep>
    ///       <e:pred name="ListSegment"><code>head</code><code>null</code><e:st n="L"/></e:pred>
    ///       <e:pred name="EmptyList"><code>null</code><m:empty/></e:pred>
    ///     </e:sep>
    ///     <e:pred name="SegmentCompose"><e:st n="L"/><m:empty/><e:st n="S"/></e:pred>
    ///     <e:pred name="SetLT"><e:st n="L"/><code>value</code></e:pred>
    ///   </e:and></e:indent></e:expr>
    /// </e:exists>

    // <e:logimpl>
    //   <e:pred name="SegmentCompose"><e:st n="L"/><m:empty/><e:st n="S"/></e:pred>
    //   <e:eq><e:union><e:st n="L"/><m:empty/></e:union><e:st n="S"/></e:eq>
    //   <e:eq><e:st n="L"/><e:st n="S"/></e:eq>
    // </e:logimpl>.
    /// <e:and type="lines">
    ///   <e:sep>
    ///     <e:pred name="ListSegment"><code>head</code><code>null</code><e:st n="S"/></e:pred>
    ///     <e:pred name="EmptyList"><code>null</code><m:empty/></e:pred>
    ///   </e:sep>
    ///   <e:pred name="SegmentCompose"><e:st n="S"/><m:empty/><e:st n="S"/></e:pred>
    ///   <e:pred name="SetLT"><e:st n="S"/><code>value</code></e:pred>
    /// </e:and>

    // Discard <e:pred name="SegmentCompose"><e:st n="S"/><m:empty/><e:st n="S"/></e:pred>.
    // Discard <e:pred name="EmptyList"><code>null</code><m:empty/></e:pred>.
    /// <e:and>
    ///   <e:pred name="ListSegment"><code>head</code><code>null</code><e:st n="S"/></e:pred>
    ///   <e:pred name="SetLT"><e:st n="S"/><code>value</code></e:pred>
    /// </e:and>

    // <e:logimpl><e:pred name="ListSegment"><code>head</code><code>null</code><e:st n="S"/></e:pred><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:logimpl>.<br />
    // <e:logimpl><e:pred name="SetLT"><e:st n="L"/><code>value</code></e:pred><e:notin><code>value</code><e:st n="L"/></e:notin></e:logimpl>.
    /// <e:and>
    ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    ///   <e:notin>
    ///     <code>value</code>
    ///     <e:st n="S"/>
    ///   </e:notin>
    /// </e:and>

    o = false;
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
    // Deny if-condition.  Eliminate <code>null</code> side of disjunction.
    /// <e:exists>
    ///   <e:vars><e:st n="L"/><e:st n="R"/><e:var n="v"/><e:var n="tail"/><e:st n="T"/></e:vars>
    ///   <e:expr><e:indent><e:and type="lines">
    ///     <e:sep>
    ///       <e:pred name="ListSegment"><code>head</code><code>i</code><e:st n="L"/></e:pred>
    ///       <e:fcell>
    ///         <code>i</code>
    ///         <e:list><e:var n="v" /><e:var n="tail"/></e:list>
    ///       </e:fcell>
    ///       <e:pred name="List"><e:var n="tail"/><e:st n="T"/></e:pred>
    ///     </e:sep>
    ///     <e:pred name="ListCompose"><e:var n="v"/><e:st n="T"/><e:st n="R"/></e:pred>
    ///     <e:pred name="SegmentCompose"><e:st n="L"/><e:st n="R"/><e:st n="S"/></e:pred>
    ///     <e:pred name="SetLT"><e:st n="L"/><code>value</code></e:pred>
    ///   </e:and></e:indent></e:expr>
    /// </e:exists>

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