module ll.search.recursive;

import ll.node;

bool search(Node* head, int value) {
  /// <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
  bool o;
  if (head == null) {
    // Assert if-condition.
    /// <e:and>
    ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    ///   <e:eq>
    ///     <code>head</code>
    ///     <m:null/>
    ///   </e:eq>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:and>
    //     <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    //     <e:eq><code>head</code><m:null/></e:eq>
    //   </e:and>
    //   <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>
    // </e:logimpl>.
    /// <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>

    // Lemma: <e:logimpl>
    //   <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>
    //   <e:notin>
    //     <code>value</code>
    //     <e:st n="S"/>
    //   </e:notin>
    // </e:logimpl>.
    /// <e:and>
    ///   <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>
    ///   <e:notin>
    ///     <code>value</code>
    ///     <e:st n="S"/>
    ///   </e:notin>
    /// </e:and>

    // Weakening lemma: <e:logimpl>
    //   <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>
    //   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    // </e:logimpl>.
    /// <e:and>
    ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    ///   <e:notin>
    ///     <code>value</code>
    ///     <e:st n="S"/>
    ///   </e:notin>
    /// </e:and>

    o = false;
    // Assignment.
    /// <e:and>
    ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    ///   <e:and>
    ///       <e:notin>
    ///         <code>value</code>
    ///         <e:st n="S"/>
    ///       </e:notin>
    ///       <e:eq>
    ///         <code>o</code>
    ///         <code>false</code>
    ///       </e:eq>
    ///   </e:and>
    /// </e:and>

    // <e:doubleimpl>falsefalse</e:doubleimpl>.
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
    /// <e:and>
    ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    ///   <e:noteq>
    ///     <code>head</code>
    ///     <m:null/>
    ///   </e:noteq>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:and>
    //     <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    //     <e:noteq>
    //       <code>head</code>
    //       <m:null/>
    //     </e:noteq>
    //   </e:and>
    //   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
    // </e:logimpl>.
    /// <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>

    // Open <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>.
    /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
    /// <e:indent>
    ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
    ///   <e:sep>
    ///     <e:fcell>
    ///       <code>head</code>
    ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
    ///     </e:fcell>
    ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
    ///   </e:sep>
    /// </e:indent>

    if (head.value == value) {
      // Assert if-condition: substitute <code>value</code> for <e:var n="v"/>.
      /// ∃<e:var n="tail"/>, <e:st n="T"/>.<br />
      /// <e:indent>
      ///   <e:pred name="ListCompose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
      ///   <e:sep>
      ///     <e:fcell>
      ///       <code>head</code>
      ///       <e:list><code>value</code>, <e:var n="tail"/></e:list>
      ///     </e:fcell>
      ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
      ///   </e:sep>
      /// </e:indent>

      // <e:logimpl>
      //   <e:pred name="ListCompose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred>
      //   <e:in>
      //     <code>value</code>
      //     <e:st n="S"/>
      //   </e:in>
      // </e:logimpl>.
      /// ∃<e:var n="tail"/>, <e:st n="T"/>.<br />
      /// <e:indent>
      ///   <e:pred name="ListCompose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
      ///   <e:sep>
      ///     <e:fcell>
      ///       <code>head</code>
      ///       <e:list><code>value</code>, <e:var n="tail"/></e:list>
      ///     </e:fcell>
      ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
      ///   </e:sep> ∧<br />
      ///   <e:in>
      ///     <code>value</code>
      ///     <e:st n="S"/>
      ///   </e:in>
      /// </e:indent>

      // Close <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>.
      /// <e:and>
      ///   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
      ///   <e:in>
      ///     <code>value</code>
      ///     <e:st n="S"/>
      ///   </e:in>
      /// </e:and>

      // Weakening lemma: <e:logimpl>
      //   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
      //   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
      // </e:logimpl>.
      /// <e:and>
      ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
      ///   <e:in>
      ///     <code>value</code>
      ///     <e:st n="S"/>
      ///   </e:in>
      /// </e:and>

      o = true;
      // Assignment.
      /// <e:and>
      ///   <e:and>
      ///     <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
      ///     <e:in>
      ///       <code>value</code>
      ///       <e:st n="S"/>
      ///     </e:in>
      ///   </e:and>
      ///   <e:eq>
      ///     <code>o</code>
      ///     <code>true</code>
      ///   </e:eq>
      /// </e:and>

      // <e:doubleimpl>truetrue</e:doubleimpl>.
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
      /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
      /// <e:indent>
      ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
      ///   <e:sep>
      ///     <e:fcell>
      ///       <code>head</code>
      ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
      ///     </e:fcell>
      ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
      ///   </e:sep> ∧<br />
      ///   <e:noteq>
      ///     <e:var n="v"/>
      ///     <code>value</code>
      ///   </e:noteq>
      /// </e:indent>

      if (head.value < value) {
        // Assert if-condition.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:var n="v"/>
        ///     <code>value</code>
        ///   </e:lt>
        /// </e:indent>

        // Lemma:
        // <e:logimpl>
        //   <e:and>
        //     <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred>
        //     <e:lt>
        //       <e:var n="v"/>
        //       <code>value</code>
        //     </e:lt>
        //   </e:and>
        //   <e:doubleimpl>
        //     <e:in>
        //       <code>value</code>
        //       <e:st n="T"/>
        //     </e:in>
        //     <e:in>
        //       <code>value</code>
        //       <e:st n="S"/>
        //     </e:in>
        //   </e:doubleimpl>
        // </e:logimpl>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:var n="v"/>
        ///     <code>value</code>
        ///   </e:lt> ∧<br />
        ///   <e:doubleimpl>
        ///     <e:in>
        ///       <code>value</code>
        ///       <e:st n="T"/>
        ///     </e:in>
        ///     <e:in>
        ///       <code>value</code>
        ///       <e:st n="S"/>
        ///     </e:in>
        ///   </e:doubleimpl>
        /// </e:indent>

        o = search(head.tail, value);
        // Inductive use of specification.
        // Note <e:lt><e:size><e:st n="T"/></e:size><e:size><e:st n="S"/></e:size></e:lt>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:var n="v"/>
        ///     <code>value</code>
        ///   </e:lt> ∧<br />
        ///   <e:doubleimpl>
        ///     <e:in>
        ///       <code>value</code>
        ///       <e:st n="T"/>
        ///     </e:in>
        ///     <e:in>
        ///       <code>value</code>
        ///       <e:st n="S"/>
        ///     </e:in>
        ///   </e:doubleimpl> ∧<br />
        ///   <e:doubleimpl>
        ///     <code>o</code>
        ///     <e:in>
        ///       <code>value</code>
        ///       <e:st n="T"/>
        ///     </e:in>
        ///   </e:doubleimpl>
        /// </e:indent>

        // Transitivity of double implication.  Discard unrequired assertions.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
        ///   </e:sep> ∧<br />
        ///   <e:doubleimpl>
        ///     <code>o</code>
        ///     <e:in>
        ///       <code>value</code>
        ///       <e:st n="S"/>
        ///     </e:in>
        ///   </e:doubleimpl>
        /// </e:indent>

        // Close <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>.
        /// <e:and>
        ///   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
        ///   <e:doubleimpl>
        ///     <code>o</code>
        ///     <e:in>
        ///       <code>value</code>
        ///       <e:st n="S"/>
        ///     </e:in>
        ///   </e:doubleimpl>
        /// </e:and>

        // Weakening: <e:logimpl>
        //   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
        //   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
        // </e:logimpl>.
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
        // Use <e:logimpl>
        //   ¬(<e:lt>
        //     <e:var n="v"/>
        //     <code>value</code>
        //   </e:lt>)
        //   <e:leq>
        //     <code>value</code>
        //     <e:var n="v"/>
        //   </e:leq>
        // </e:logimpl>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
        ///   </e:sep> ∧<br />
        ///   <e:and>
        ///     <e:noteq>
        ///       <code>value</code>
        ///       <e:var n="v"/>
        ///     </e:noteq>
        ///     <e:leq>
        ///       <code>value</code>
        ///       <e:var n="v"/>
        ///     </e:leq>
        ///   </e:and>
        /// </e:indent>

        // <e:logimpl>
        //   (<e:and>
        //     <e:noteq>
        //       <e:var n="a"/>
        //       <e:var n="b"/>
        //     </e:noteq>
        //     <e:leq>
        //       <e:var n="a"/>
        //       <e:var n="b"/>
        //     </e:leq>
        //   </e:and>)
        //   <e:lt>
        //     <e:var n="b"/>
        //     <e:var n="a"/>
        //   </e:lt>
        //  </e:logimpl>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <code>value</code>
        ///     <e:var n="v"/>
        ///   </e:lt>
        /// </e:indent>

        // Lemma: <e:logimpl>
        //   (<e:and>
        //     <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred>
        //     <e:lt>
        //       <code>value</code>
        //       <e:var n="v"/>
        //     </e:lt>
        //   </e:and>)
        //   <e:notin>
        //     <code>value</code>
        //     <e:st n="S"/>
        //   </e:notin>
        // </e:logimpl>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
        ///   </e:sep> ∧<br />
        ///   <e:notin>
        ///     <code>value</code>
        ///     <e:st n="S"/>
        ///   </e:notin>
        /// </e:indent>

        // Close <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>.
        /// <e:and>
        ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
        ///   <e:notin>
        ///     <code>value</code>
        ///     <e:st n="S"/>
        ///   </e:notin>
        /// </e:and>

        o = false;
        // Assignment.
        /// <e:and>
        ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
        ///   <e:and>
        ///     <e:notin>
        ///       <code>value</code>
        ///       <e:st n="S"/>
        ///     </e:notin>
        ///     <e:eq>
        ///       <code>o</code>
        ///       <code>false</code>
        ///     </e:eq>
        ///   </e:and>
        /// </e:and>

        // <e:doubleimpl>falsefalse</e:doubleimpl>.
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