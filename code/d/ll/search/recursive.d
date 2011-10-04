module ll.search.recursive;

import ll.node;

bool search(Node* head, int value) {
  /// <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
  bool o;
  if (head == null) {
    // Assert if-condition.
    /// <e:and>
    ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:eq>
    ///     <e:fst><code>head</code></e:fst>
    ///     <e:snd><code>null</code></e:snd>
    ///   </e:eq></e:snd>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:fst><e:and>
    //     <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    //     <e:snd><e:eq><e:fst><code>head</code></e:fst><e:snd><code>null</code></e:snd></e:eq></e:snd>
    //   </e:and></e:fst>
    //   <e:snd><e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
    // </e:logimpl>.
    /// <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>

    // Lemma: <e:logimpl>
    //   <e:fst><e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    //   <e:snd><e:notin>
    //     <e:fst><code>value</code></e:fst>
    //     <e:snd><e:st n="S"/></e:snd>
    //   </e:notin></e:snd>
    // </e:logimpl>.
    /// <e:and>
    ///   <e:fst><e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:notin>
    ///     <e:fst><code>value</code></e:fst>
    ///     <e:snd><e:st n="S"/></e:snd>
    ///   </e:notin></e:snd>
    /// </e:and>

    // Weakening lemma: <e:logimpl>
    //   <e:fst><e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    //   <e:snd><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
    // </e:logimpl>.
    /// <e:and>
    ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:notin>
    ///     <e:fst><code>value</code></e:fst>
    ///     <e:snd><e:st n="S"/></e:snd>
    ///   </e:notin></e:snd>
    /// </e:and>

    o = false;
    // Assignment.
    /// <e:and>
    ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:and>
    ///     <e:fst>
    ///       <e:notin>
    ///         <e:fst><code>value</code></e:fst>
    ///         <e:snd><e:st n="S"/></e:snd>
    ///       </e:notin>
    ///     </e:fst>
    ///     <e:snd>
    ///       <e:eq>
    ///         <e:fst><code>o</code></e:fst>
    ///         <e:snd><code>false</code></e:snd>
    ///       </e:eq>
    ///     </e:snd>
    ///   </e:and></e:snd>
    /// </e:and>

    // <e:doubleimpl><e:fst>false</e:fst><e:snd>false</e:snd></e:doubleimpl>.
    /// <e:and>
    ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:doubleimpl>
    ///     <e:fst><code>o</code></e:fst>
    ///     <e:snd><e:in>
    ///       <e:fst><code>value</code></e:fst>
    ///       <e:snd><e:st n="S"/></e:snd>
    ///     </e:in></e:snd>
    ///   </e:doubleimpl></e:snd>
    /// </e:and>
  }
  else {
    // Deny if-condition.
    /// <e:and>
    ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:noteq>
    ///     <e:fst><code>head</code></e:fst>
    ///     <e:snd><code>null</code></e:snd>
    ///   </e:noteq></e:snd>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:fst><e:and>
    //     <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    //     <e:snd><e:noteq>
    //       <e:fst><code>head</code></e:fst>
    //       <e:snd><code>null</code></e:snd>
    //     </e:noteq></e:snd>
    //   </e:and></e:fst>
    //   <e:snd><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
    // </e:logimpl>.
    /// <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>

    // Open <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>.
    /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
    /// <e:indent>
    ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
    ///   <e:sep>
    ///     <e:fst><e:fcell>
    ///       <e:fst><code>head</code></e:fst>
    ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
    ///     </e:fcell></e:fst>
    ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
    ///   </e:sep>
    /// </e:indent>

    if (head.value == value) {
      // Assert if-condition: substitute <code>value</code> for <e:var n="v"/>.
      /// ∃<e:var n="tail"/>, <e:st n="T"/>.<br />
      /// <e:indent>
      ///   <e:pred name="ListCompose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
      ///   <e:sep>
      ///     <e:fst><e:fcell>
      ///       <e:fst><code>head</code></e:fst>
      ///       <e:snd><code>value</code>, <e:var n="tail"/></e:snd>
      ///     </e:fcell></e:fst>
      ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
      ///   </e:sep>
      /// </e:indent>

      // <e:logimpl>
      //   <e:fst><e:pred name="ListCompose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred></e:fst>
      //   <e:snd><e:in>
      //     <e:fst><code>value</code></e:fst>
      //     <e:snd><e:st n="S"/></e:snd>
      //   </e:in></e:snd>
      // </e:logimpl>.
      /// ∃<e:var n="tail"/>, <e:st n="T"/>.<br />
      /// <e:indent>
      ///   <e:pred name="ListCompose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
      ///   <e:sep>
      ///     <e:fst><e:fcell>
      ///       <e:fst><code>head</code></e:fst>
      ///       <e:snd><code>value</code>, <e:var n="tail"/></e:snd>
      ///     </e:fcell></e:fst>
      ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
      ///   </e:sep> ∧<br />
      ///   <e:in>
      ///     <e:fst><code>value</code></e:fst>
      ///     <e:snd><e:st n="S"/></e:snd>
      ///   </e:in>
      /// </e:indent>

      // Close <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>.
      /// <e:and>
      ///   <e:fst><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:in>
      ///     <e:fst><code>value</code></e:fst>
      ///     <e:snd><e:st n="S"/></e:snd>
      ///   </e:in></e:snd>
      /// </e:and>

      // Weakening lemma: <e:logimpl>
      //   <e:fst><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
      //   <e:snd><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
      // </e:logimpl>.
      /// <e:and>
      ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:in>
      ///     <e:fst><code>value</code></e:fst>
      ///     <e:snd><e:st n="S"/></e:snd>
      ///   </e:in></e:snd>
      /// </e:and>

      o = true;
      // Assignment.
      /// <e:and>
      ///   <e:fst><e:and>
      ///     <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:in>
      ///       <e:fst><code>value</code></e:fst>
      ///       <e:snd><e:st n="S"/></e:snd>
      ///     </e:in></e:snd>
      ///   </e:and></e:fst>
      ///   <e:snd><e:eq>
      ///     <e:fst><code>o</code></e:fst>
      ///     <e:snd><code>true</code></e:snd>
      ///   </e:eq></e:snd>
      /// </e:and>

      // <e:doubleimpl><e:fst>true</e:fst><e:snd>true</e:snd></e:doubleimpl>.
      /// <e:and>
      ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:doubleimpl>
      ///     <e:fst><code>o</code></e:fst>
      ///     <e:snd><e:in>
      ///       <e:fst><code>value</code></e:fst>
      ///       <e:snd><e:st n="S"/></e:snd>
      ///     </e:in></e:snd>
      ///   </e:doubleimpl></e:snd>
      /// </e:and>
    }
    else {
      // Deny if-condition.
      /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
      /// <e:indent>
      ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
      ///   <e:sep>
      ///     <e:fst><e:fcell>
      ///       <e:fst><code>head</code></e:fst>
      ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
      ///     </e:fcell></e:fst>
      ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
      ///   </e:sep> ∧<br />
      ///   <e:noteq>
      ///     <e:fst><e:var n="v"/></e:fst>
      ///     <e:snd><code>value</code></e:snd>
      ///   </e:noteq>
      /// </e:indent>

      if (head.value < value) {
        // Assert if-condition.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:fst><e:var n="v"/></e:fst>
        ///     <e:snd><code>value</code></e:snd>
        ///   </e:lt>
        /// </e:indent>

        // Lemma:
        // <e:logimpl>
        //   <e:fst><e:and>
        //     <e:fst><e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred></e:fst>
        //     <e:snd><e:lt>
        //       <e:fst><e:var n="v"/></e:fst>
        //       <e:snd><code>value</code></e:snd>
        //     </e:lt></e:snd>
        //   </e:and></e:fst>
        //   <e:snd><e:doubleimpl>
        //     <e:fst><e:in>
        //       <e:fst><code>value</code></e:fst>
        //       <e:snd><e:st n="T"/></e:snd>
        //     </e:in></e:fst>
        //     <e:snd><e:in>
        //       <e:fst><code>value</code></e:fst>
        //       <e:snd><e:st n="S"/></e:snd>
        //     </e:in></e:snd>
        //   </e:doubleimpl></e:snd>
        // </e:logimpl>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:fst><e:var n="v"/></e:fst>
        ///     <e:snd><code>value</code></e:snd>
        ///   </e:lt> ∧<br />
        ///   <e:doubleimpl>
        ///     <e:fst><e:in>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:st n="T"/></e:snd>
        ///     </e:in></e:fst>
        ///     <e:snd><e:in>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:st n="S"/></e:snd>
        ///     </e:in></e:snd>
        ///   </e:doubleimpl>
        /// </e:indent>

        o = search(head.tail, value);
        // Inductive use of specification.
        // Note <e:lt><e:fst><e:size><e:st n="T"/></e:size></e:fst><e:snd><e:size><e:st n="S"/></e:size></e:snd></e:lt>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:fst><e:var n="v"/></e:fst>
        ///     <e:snd><code>value</code></e:snd>
        ///   </e:lt> ∧<br />
        ///   <e:doubleimpl>
        ///     <e:fst><e:in>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:st n="T"/></e:snd>
        ///     </e:in></e:fst>
        ///     <e:snd><e:in>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:st n="S"/></e:snd>
        ///     </e:in></e:snd>
        ///   </e:doubleimpl> ∧<br />
        ///   <e:doubleimpl>
        ///     <e:fst><code>o</code></e:fst>
        ///     <e:snd><e:in>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:st n="T"/></e:snd>
        ///     </e:in></e:snd>
        ///   </e:doubleimpl>
        /// </e:indent>

        // Transitivity of double implication.  Discard unrequired assertions.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:doubleimpl>
        ///     <e:fst><code>o</code></e:fst>
        ///     <e:snd><e:in>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:st n="S"/></e:snd>
        ///     </e:in></e:snd>
        ///   </e:doubleimpl>
        /// </e:indent>

        // Close <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>.
        /// <e:and>
        ///   <e:fst><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
        ///   <e:snd><e:doubleimpl>
        ///     <e:fst><code>o</code></e:fst>
        ///     <e:snd><e:in>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:st n="S"/></e:snd>
        ///     </e:in></e:snd>
        ///   </e:doubleimpl></e:snd>
        /// </e:and>

        // Weakening: <e:logimpl>
        //   <e:fst><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
        //   <e:snd><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
        // </e:logimpl>.
        /// <e:and>
        ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
        ///   <e:snd><e:doubleimpl>
        ///     <e:fst><code>o</code></e:fst>
        ///     <e:snd><e:in>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:st n="S"/></e:snd>
        ///     </e:in></e:snd>
        ///   </e:doubleimpl></e:snd>
        /// </e:and>
      }
      else {
        // Deny if-condition.
        // Use <e:logimpl>
        //   <e:fst>¬(<e:lt>
        //     <e:fst><e:var n="v"/></e:fst>
        //     <e:snd><code>value</code></e:snd>
        //   </e:lt>)</e:fst>
        //   <e:snd><e:leq>
        //     <e:fst><code>value</code></e:fst>
        //     <e:snd><e:var n="v"/></e:snd>
        //   </e:leq></e:snd>
        // </e:logimpl>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:and>
        ///     <e:fst><e:noteq>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:var n="v"/></e:snd>
        ///     </e:noteq></e:fst>
        ///     <e:snd><e:leq>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:var n="v"/></e:snd>
        ///     </e:leq></e:snd>
        ///   </e:and>
        /// </e:indent>

        // <e:logimpl>
        //   <e:fst>(<e:and>
        //     <e:fst><e:noteq>
        //       <e:fst><e:var n="a"/></e:fst>
        //       <e:snd><e:var n="b"/></e:snd>
        //     </e:noteq></e:fst>
        //     <e:snd><e:leq>
        //       <e:fst><e:var n="a"/></e:fst>
        //       <e:snd><e:var n="b"/></e:snd>
        //     </e:leq></e:snd>
        //   </e:and>)</e:fst>
        //   <e:snd><e:lt>
        //     <e:fst><e:var n="b"/></e:fst>
        //     <e:snd><e:var n="a"/></e:snd>
        //   </e:lt></e:snd>
        //  </e:logimpl>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:fst><code>value</code></e:fst>
        ///     <e:snd><e:var n="v"/></e:snd>
        ///   </e:lt>
        /// </e:indent>

        // Lemma: <e:logimpl>
        //   <e:fst>(<e:and>
        //     <e:fst><e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred></e:fst>
        //     <e:snd><e:lt>
        //       <e:fst><code>value</code></e:fst>
        //       <e:snd><e:var n="v"/></e:snd>
        //     </e:lt></e:snd>
        //   </e:and>)</e:fst>
        //   <e:snd><e:notin>
        //     <e:fst><code>value</code></e:fst>
        //     <e:snd><e:st n="S"/></e:snd>
        //   </e:notin></e:snd>
        // </e:logimpl>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:notin>
        ///     <e:fst><code>value</code></e:fst>
        ///     <e:snd><e:st n="S"/></e:snd>
        ///   </e:notin>
        /// </e:indent>

        // Close <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>.
        /// <e:and>
        ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
        ///   <e:snd><e:notin>
        ///     <e:fst><code>value</code></e:fst>
        ///     <e:snd><e:st n="S"/></e:snd>
        ///   </e:notin></e:snd>
        /// </e:and>

        o = false;
        // Assignment.
        /// <e:and>
        ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
        ///   <e:snd><e:and>
        ///     <e:fst><e:notin>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:st n="S"/></e:snd>
        ///     </e:notin></e:fst>
        ///     <e:snd><e:eq>
        ///       <e:fst><code>o</code></e:fst>
        ///       <e:snd><code>false</code></e:snd>
        ///     </e:eq></e:snd>
        ///   </e:and></e:snd>
        /// </e:and>

        // <e:doubleimpl><e:fst>false</e:fst><e:snd>false</e:snd></e:doubleimpl>.
        /// <e:and>
        ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
        ///   <e:snd><e:doubleimpl>
        ///     <e:fst><code>o</code></e:fst>
        ///     <e:snd><e:in>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:st n="S"/></e:snd>
        ///     </e:in></e:snd>
        ///   </e:doubleimpl></e:snd>
        /// </e:and>
      }
      // If-rule.
      /// <e:and>
      ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:doubleimpl>
      ///     <e:fst><code>o</code></e:fst>
      ///     <e:snd><e:in>
      ///       <e:fst><code>value</code></e:fst>
      ///       <e:snd><e:st n="S"/></e:snd>
      ///     </e:in></e:snd>
      ///   </e:doubleimpl></e:snd>
      /// </e:and>
    }
    // If-rule.
    /// <e:and>
    ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:doubleimpl>
    ///     <e:fst><code>o</code></e:fst>
    ///     <e:snd><e:in>
    ///       <e:fst><code>value</code></e:fst>
    ///       <e:snd><e:st n="S"/></e:snd>
    ///     </e:in></e:snd>
    ///   </e:doubleimpl></e:snd>
    /// </e:and>
  }
  // If-rule.
  /// <e:and>
  ///   <e:fst><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
  ///   <e:snd><e:doubleimpl>
  ///     <e:fst><code>o</code></e:fst>
  ///     <e:snd><e:in>
  ///       <e:fst><code>value</code></e:fst>
  ///       <e:snd><e:st n="S"/></e:snd>
  ///     </e:in></e:snd>
  ///   </e:doubleimpl></e:snd>
  /// </e:and>

  return o;
}