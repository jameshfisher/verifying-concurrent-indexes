module ll.remove.recursive;

import ll.node;

Node* remove(Node* head, int value) {
  Node* o;
  // Precondition.
  /// <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>

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
    //     <e:snd><e:eq>
    //       <e:fst><code>head</code></e:fst>
    //       <e:snd><code>null</code></e:snd>
    //     </e:eq></e:snd>
    //   </e:and></e:fst>
    //   <e:snd><e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
    // </e:logimpl>
    /// <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>

    // <e:logimpl>
    //   <e:fst><e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    //   <e:snd><e:eq><e:fst><e:st n="S"/></e:fst><e:snd><m:scemp/></e:snd></e:eq></e:snd>
    // </e:logimpl>
    /// <e:and>
    ///   <e:fst><e:pred name="EmptyList"><code>head</code>, <e:st n="S" /></e:pred></e:fst>
    ///   <e:snd><e:eq><e:fst><e:st n="S"/></e:fst><e:snd><m:scemp/></e:snd></e:eq></e:snd>
    /// </e:and>

    // <e:eq><e:fst><e:setminus><e:fst><m:scemp/></e:fst><e:snd><e:st n="X"/></e:snd></e:setminus></e:fst><e:snd><m:scemp/></e:snd></e:eq>,
    // <e:eq><e:fst><e:st n="S"/></e:fst><e:snd><m:scemp/></e:snd></e:eq>,
    // substitution.
    /// <e:pred name="EmptyList">
    ///   <code>head</code>,
    ///     <e:setminus>
    ///       <e:fst><e:st n="S" /></e:fst>
    ///       <e:snd><e:set><code>value</code></e:set></e:snd>
    ///     </e:setminus>
    /// </e:pred>

    // Weakening lemma: <e:logimpl>
    //   <e:fst><e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
    //   <e:snd><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
    // </e:logimpl>.
    /// <e:pred name="List">
    ///   <code>head</code>,
    ///     <e:setminus>
    ///       <e:fst><e:st n="S" /></e:fst>
    ///       <e:snd><e:set><code>value</code></e:set></e:snd>
    ///     </e:setminus>
    /// </e:pred>

    o = head;
    // Assignment.
    /// <e:pred name="List">
    ///   <code>o</code>,
    ///     <e:setminus>
    ///       <e:fst><e:st n="S" /></e:fst>
    ///       <e:snd><e:set><code>value</code></e:set></e:snd>
    ///     </e:setminus>
    /// </e:pred>
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

      // Lemma: <e:logimpl>
      //   <e:fst><e:pred name="ListCompose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred></e:fst>
      //   <e:snd><e:eq>
      //     <e:fst><e:st n="T"/></e:fst>
      //     <e:snd><e:setminus>
      //       <e:fst><e:st n="S"/></e:fst>
      //       <e:snd><e:set><code>value</code></e:set></e:snd>
      //     </e:setminus></e:snd>
      //   </e:eq></e:snd>
      // </e:logimpl>.  Discard <e:pred name="ListCompose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred>.
      /// ∃<e:var n="tail"/>, <e:st n="T"/>.<br />
      /// <e:indent>
      ///   <e:sep>
      ///     <e:fst><e:fcell>
      ///       <e:fst><code>head</code></e:fst>
      ///       <e:snd><code>value</code>, <e:var n="tail"/></e:snd>
      ///     </e:fcell></e:fst>
      ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
      ///   </e:sep> ∧<br />
      ///   <e:eq>
      ///     <e:fst><e:st n="T"/></e:fst>
      ///     <e:snd><e:setminus>
      ///       <e:fst><e:st n="S"/></e:fst>
      ///       <e:snd><e:set><code>value</code></e:set></e:snd>
      ///     </e:setminus></e:snd>
      ///   </e:eq>
      /// </e:indent>

      // Substitution.  Discard <e:eq>
      //     <e:fst><e:st n="T"/></e:fst>
      //     <e:snd><e:setminus>
      //       <e:fst><e:st n="S"/></e:fst>
      //       <e:snd><e:set><code>value</code></e:set></e:snd>
      //     </e:setminus></e:snd>
      //   </e:eq>.
      /// ∃<e:var n="tail"/>.<br />
      /// <e:indent>
      ///   <e:sep>
      ///     <e:fst><e:fcell>
      ///       <e:fst><code>head</code></e:fst>
      ///       <e:snd><code>value</code>, <e:var n="tail"/></e:snd>
      ///     </e:fcell></e:fst>
      ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred></e:snd>
      ///   </e:sep>
      /// </e:indent>

      o = head.tail;
      // Assignment.
      /// <e:sep>
      ///   <e:fst><e:fcell>
      ///     <e:fst><code>head</code></e:fst>
      ///     <e:snd><code>value</code>, <code>o</code></e:snd>
      ///   </e:fcell></e:fst>
      ///   <e:snd><e:pred name="List"><code>o</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred></e:snd>
      /// </e:sep>

      delete head;
      // Release heap chunk.
      /// <e:pred name="List">
      ///   <code>o</code>,
      ///     <e:setminus>
      ///       <e:fst><e:st n="S" /></e:fst>
      ///       <e:snd><e:set><code>value</code></e:set></e:snd>
      ///     </e:setminus>
      /// </e:pred>
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

      if (head.value > value) {
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
        
        Node* ntail = remove(head.tail, value);
        // Use specification for <code>remove</code>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><code>ntail</code>, <e:setminus><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:fst><e:var n="v"/></e:fst>
        ///     <e:snd><code>value</code></e:snd>
        ///   </e:lt>
        /// </e:indent>

        head.tail = ntail;
        // Assignment.
        /// ∃<e:var n="v"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <code>ntail</code></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><code>ntail</code>, <e:setminus><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:fst><e:var n="v"/></e:fst>
        ///     <e:snd><code>value</code></e:snd>
        ///   </e:lt>
        /// </e:indent>

        // <m:existsIntro/> on <code>ntail</code> as <e:var n="tail"/>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:setminus><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:fst><e:var n="v"/></e:fst>
        ///     <e:snd><code>value</code></e:snd>
        ///   </e:lt>
        /// </e:indent>

        // Lemma: <e:logimpl>
        //   <e:fst><e:and>
        //     <e:fst><e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred></e:fst>
        //     <e:snd><e:noteq>
        //       <e:fst><e:var n="v"/></e:fst>
        //       <e:snd><code>value</code></e:snd>
        //     </e:noteq></e:snd>
        //   </e:and></e:fst>
        //   <e:snd><e:pred name="ListCompose"><e:var n="v"/>, <e:setminus><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus>, <e:setminus><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred></e:snd>
        // </e:logimpl>.  Discard <e:lt><e:fst><e:var n="v"/></e:fst><e:snd><code>value</code></e:snd></e:lt>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:setminus><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus>, <e:setminus><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:setminus><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred></e:snd>
        ///   </e:sep>
        /// </e:indent>

        // <m:existsIntro/> on <e:setminus><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus> as <e:st n="T"/>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:setminus><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
        ///   </e:sep>
        /// </e:indent>

        // Close <e:pred name="NonEmptyList">
        //   <code>head</code>,
        //     <e:setminus>
        //       <e:fst><e:st n="S" /></e:fst>
        //       <e:snd><e:set><code>value</code></e:set></e:snd>
        //     </e:setminus>
        // </e:pred>.
        /// <e:pred name="NonEmptyList">
        ///   <code>head</code>,
        ///     <e:setminus>
        ///       <e:fst><e:st n="S" /></e:fst>
        ///       <e:snd><e:set><code>value</code></e:set></e:snd>
        ///     </e:setminus>
        /// </e:pred>

        // Weakening lemma: <e:logimpl>
        //   <e:fst><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
        //   <e:snd><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
        // </e:logimpl>.
        /// <e:pred name="List">
        ///   <code>head</code>,
        ///     <e:setminus>
        ///       <e:fst><e:st n="S" /></e:fst>
        ///       <e:snd><e:set><code>value</code></e:set></e:snd>
        ///     </e:setminus>
        /// </e:pred>

        o = head;
        // Assignment.
        /// <e:pred name="List">
        ///   <code>o</code>,
        ///     <e:setminus>
        ///       <e:fst><e:st n="S" /></e:fst>
        ///       <e:snd><e:set><code>value</code></e:set></e:snd>
        ///     </e:setminus>
        /// </e:pred>
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
        //   <e:fst><e:and>
        //     <e:fst><e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred></e:fst>
        //     <e:snd><e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt></e:snd>
        //   </e:and></e:fst>
        //   <e:snd><e:notin><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:notin></e:snd>
        // </e:logimpl>.  Discard <e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt>.
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

        // Close <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>.
        /// <e:and>
        ///   <e:fst><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
        ///   <e:snd><e:notin>
        ///     <e:fst><code>value</code></e:fst>
        ///     <e:snd><e:st n="S"/></e:snd>
        ///   </e:notin></e:snd>
        /// </e:and>

        // <e:logimpl>
        //   <e:fst><e:notin><e:fst><e:var n="a"/></e:fst><e:snd><e:st n="X"/></e:snd></e:notin></e:fst>
        //   <e:snd><e:eq>
        //     <e:fst><e:setminus><e:fst><e:st n="X"/></e:fst><e:snd><e:set><e:var n="a"/></e:set></e:snd></e:setminus></e:fst>
        //     <e:snd><e:st n="X"/></e:snd>
        //   </e:eq></e:snd>
        // </e:logimpl>.  Discard <e:notin><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:notin>.
        /// <e:and>
        ///   <e:fst><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
        ///   <e:snd><e:eq>
        ///     <e:fst><e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:fst>
        ///     <e:snd><e:st n="S"/></e:snd>
        ///   </e:eq></e:snd>
        /// </e:and>

        // Substitution.  Discard <e:eq>
        //   <e:fst><e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:fst>
        //   <e:snd><e:st n="S"/></e:snd>
        // </e:eq>.
        /// <e:pred name="NonEmptyList">
        ///   <code>head</code>,
        ///     <e:setminus>
        ///       <e:fst><e:st n="S" /></e:fst>
        ///       <e:snd><e:set><code>value</code></e:set></e:snd>
        ///     </e:setminus>
        /// </e:pred>

        // Weakening lemma: <e:logimpl>
        //   <e:fst><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
        //   <e:snd><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
        // </e:logimpl>.
        /// <e:pred name="List">
        ///   <code>head</code>,
        ///     <e:setminus>
        ///       <e:fst><e:st n="S" /></e:fst>
        ///       <e:snd><e:set><code>value</code></e:set></e:snd>
        ///     </e:setminus>
        /// </e:pred>

        o = head;
        // Assignment.
        /// <e:pred name="List">
        ///   <code>o</code>,
        ///     <e:setminus>
        ///       <e:fst><e:st n="S" /></e:fst>
        ///       <e:snd><e:set><code>value</code></e:set></e:snd>
        ///     </e:setminus>
        /// </e:pred>
      }
      // If-rule.
      /// <e:pred name="List">
      ///   <code>o</code>,
      ///     <e:setminus>
      ///       <e:fst><e:st n="S" /></e:fst>
      ///       <e:snd><e:set><code>value</code></e:set></e:snd>
      ///     </e:setminus>
      /// </e:pred>
    }
    // If-rule.
    /// <e:pred name="List">
    ///   <code>o</code>,
    ///     <e:setminus>
    ///       <e:fst><e:st n="S" /></e:fst>
    ///       <e:snd><e:set><code>value</code></e:set></e:snd>
    ///     </e:setminus>
    /// </e:pred>
  }
  // If-rule.
  /// <e:pred name="List">
  ///   <code>o</code>,
  ///     <e:setminus>
  ///       <e:fst><e:st n="S" /></e:fst>
  ///       <e:snd><e:set><code>value</code></e:set></e:snd>
  ///     </e:setminus>
  /// </e:pred>

  return o;
}