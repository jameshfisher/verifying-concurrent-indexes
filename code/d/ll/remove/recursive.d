module ll.remove.recursive;

import ll.node;

Node* remove(Node* head, int value) {
  Node* o;
  // Precondition.
  /// <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>

  if (head == null) {
    // Assert if-condition.
    /// <e:and>
    ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    ///   <e:eq>
    ///     <code>head</code>
    ///     <code>null</code>
    ///   </e:eq>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:and>
    //     <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    //     <e:eq>
    //       <code>head</code>
    //       <code>null</code>
    //     </e:eq>
    //   </e:and>
    //   <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>
    // </e:logimpl>
    /// <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>

    // <e:logimpl>
    //   <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>
    //   <e:eq><e:st n="S"/><m:scemp/></e:eq>
    // </e:logimpl>
    /// <e:and>
    ///   <e:pred name="EmptyList"><code>head</code>, <e:st n="S" /></e:pred>
    ///   <e:eq><e:st n="S"/><m:scemp/></e:eq>
    /// </e:and>

    // <e:eq><e:setminus><m:scemp/><e:st n="X"/></e:setminus><m:scemp/></e:eq>,
    // <e:eq><e:st n="S"/><m:scemp/></e:eq>,
    // substitution.
    /// <e:pred name="EmptyList">
    ///   <code>head</code>,
    ///     <e:setminus>
    ///       <e:st n="S" />
    ///       <e:set><code>value</code></e:set>
    ///     </e:setminus>
    /// </e:pred>

    // Weakening lemma: <e:logimpl>
    //   <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>
    //   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    // </e:logimpl>.
    /// <e:pred name="List">
    ///   <code>head</code>,
    ///     <e:setminus>
    ///       <e:st n="S" />
    ///       <e:set><code>value</code></e:set>
    ///     </e:setminus>
    /// </e:pred>

    o = head;
    // Assignment.
    /// <e:pred name="List">
    ///   <code>o</code>,
    ///     <e:setminus>
    ///       <e:st n="S" />
    ///       <e:set><code>value</code></e:set>
    ///     </e:setminus>
    /// </e:pred>
  }
  else {
    // Deny if-condition.
    /// <e:and>
    ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    ///   <e:noteq>
    ///     <code>head</code>
    ///     <code>null</code>
    ///   </e:noteq>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:and>
    //     <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
    //     <e:noteq>
    //       <code>head</code>
    //       <code>null</code>
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

      // Lemma: <e:logimpl>
      //   <e:pred name="ListCompose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred>
      //   <e:eq>
      //     <e:st n="T"/>
      //     <e:setminus>
      //       <e:st n="S"/>
      //       <e:set><code>value</code></e:set>
      //     </e:setminus>
      //   </e:eq>
      // </e:logimpl>.  Discard <e:pred name="ListCompose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred>.
      /// ∃<e:var n="tail"/>, <e:st n="T"/>.<br />
      /// <e:indent>
      ///   <e:sep>
      ///     <e:fcell>
      ///       <code>head</code>
      ///       <e:list><code>value</code>, <e:var n="tail"/></e:list>
      ///     </e:fcell>
      ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
      ///   </e:sep> ∧<br />
      ///   <e:eq>
      ///     <e:st n="T"/>
      ///     <e:setminus>
      ///       <e:st n="S"/>
      ///       <e:set><code>value</code></e:set>
      ///     </e:setminus>
      ///   </e:eq>
      /// </e:indent>

      // Substitution.  Discard <e:eq>
      //     <e:st n="T"/>
      //     <e:setminus>
      //       <e:st n="S"/>
      //       <e:set><code>value</code></e:set>
      //     </e:setminus>
      //   </e:eq>.
      /// ∃<e:var n="tail"/>.<br />
      /// <e:indent>
      ///   <e:sep>
      ///     <e:fcell>
      ///       <code>head</code>
      ///       <e:list><code>value</code>, <e:var n="tail"/></e:list>
      ///     </e:fcell>
      ///     <e:pred name="List"><e:var n="tail"/>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>
      ///   </e:sep>
      /// </e:indent>

      o = head.tail;
      // Assignment.
      /// <e:sep>
      ///   <e:fcell>
      ///     <code>head</code>
      ///     <e:list><code>value</code>, <code>o</code></e:list>
      ///   </e:fcell>
      ///   <e:pred name="List"><code>o</code>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>
      /// </e:sep>

      delete head;
      // Release heap chunk.
      /// <e:pred name="List">
      ///   <code>o</code>,
      ///     <e:setminus>
      ///       <e:st n="S" />
      ///       <e:set><code>value</code></e:set>
      ///     </e:setminus>
      /// </e:pred>
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

      if (head.value > value) {
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
        
        Node* ntail = remove(head.tail, value);
        // Use specification for <code>remove</code>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><code>ntail</code>, <e:setminus><e:st n="T"/><e:set><code>value</code></e:set></e:setminus></e:pred>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:var n="v"/>
        ///     <code>value</code>
        ///   </e:lt>
        /// </e:indent>

        head.tail = ntail;
        // Assignment.
        /// ∃<e:var n="v"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <code>ntail</code></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><code>ntail</code>, <e:setminus><e:st n="T"/><e:set><code>value</code></e:set></e:setminus></e:pred>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:var n="v"/>
        ///     <code>value</code>
        ///   </e:lt>
        /// </e:indent>

        // <m:existsIntro/> on <code>ntail</code> as <e:var n="tail"/>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:setminus><e:st n="T"/><e:set><code>value</code></e:set></e:setminus></e:pred>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:var n="v"/>
        ///     <code>value</code>
        ///   </e:lt>
        /// </e:indent>

        // Lemma: <e:logimpl>
        //   <e:and>
        //     <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred>
        //     <e:noteq>
        //       <e:var n="v"/>
        //       <code>value</code>
        //     </e:noteq>
        //   </e:and>
        //   <e:pred name="ListCompose"><e:var n="v"/>, <e:setminus><e:st n="T"/><e:set><code>value</code></e:set></e:setminus>, <e:setminus><e:st n="S" /><e:set><code>value</code></e:set></e:setminus></e:pred>
        // </e:logimpl>.  Discard <e:lt><e:var n="v"/><code>value</code></e:lt>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:setminus><e:st n="T"/><e:set><code>value</code></e:set></e:setminus>, <e:setminus><e:st n="S" /><e:set><code>value</code></e:set></e:setminus></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:setminus><e:st n="T"/><e:set><code>value</code></e:set></e:setminus></e:pred>
        ///   </e:sep>
        /// </e:indent>

        // <m:existsIntro/> on <e:setminus><e:st n="T"/><e:set><code>value</code></e:set></e:setminus> as <e:st n="T"/>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:setminus><e:st n="S" /><e:set><code>value</code></e:set></e:setminus></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
        ///   </e:sep>
        /// </e:indent>

        // Close <e:pred name="NonEmptyList">
        //   <code>head</code>,
        //     <e:setminus>
        //       <e:st n="S" />
        //       <e:set><code>value</code></e:set>
        //     </e:setminus>
        // </e:pred>.
        /// <e:pred name="NonEmptyList">
        ///   <code>head</code>,
        ///     <e:setminus>
        ///       <e:st n="S" />
        ///       <e:set><code>value</code></e:set>
        ///     </e:setminus>
        /// </e:pred>

        // Weakening lemma: <e:logimpl>
        //   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
        //   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
        // </e:logimpl>.
        /// <e:pred name="List">
        ///   <code>head</code>,
        ///     <e:setminus>
        ///       <e:st n="S" />
        ///       <e:set><code>value</code></e:set>
        ///     </e:setminus>
        /// </e:pred>

        o = head;
        // Assignment.
        /// <e:pred name="List">
        ///   <code>o</code>,
        ///     <e:setminus>
        ///       <e:st n="S" />
        ///       <e:set><code>value</code></e:set>
        ///     </e:setminus>
        /// </e:pred>
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
        //   <e:and>
        //     <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred>
        //     <e:lt><code>value</code><e:var n="v"/></e:lt>
        //   </e:and>
        //   <e:notin><code>value</code><e:st n="S"/></e:notin>
        // </e:logimpl>.  Discard <e:lt><code>value</code><e:var n="v"/></e:lt>.
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

        // Close <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>.
        /// <e:and>
        ///   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
        ///   <e:notin>
        ///     <code>value</code>
        ///     <e:st n="S"/>
        ///   </e:notin>
        /// </e:and>

        // <e:logimpl>
        //   <e:notin><e:var n="a"/><e:st n="X"/></e:notin>
        //   <e:eq>
        //     <e:setminus><e:st n="X"/><e:set><e:var n="a"/></e:set></e:setminus>
        //     <e:st n="X"/>
        //   </e:eq>
        // </e:logimpl>.  Discard <e:notin><code>value</code><e:st n="S"/></e:notin>.
        /// <e:and>
        ///   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
        ///   <e:eq>
        ///     <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus>
        ///     <e:st n="S"/>
        ///   </e:eq>
        /// </e:and>

        // Substitution.  Discard <e:eq>
        //   <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus>
        //   <e:st n="S"/>
        // </e:eq>.
        /// <e:pred name="NonEmptyList">
        ///   <code>head</code>,
        ///     <e:setminus>
        ///       <e:st n="S" />
        ///       <e:set><code>value</code></e:set>
        ///     </e:setminus>
        /// </e:pred>

        // Weakening lemma: <e:logimpl>
        //   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
        //   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
        // </e:logimpl>.
        /// <e:pred name="List">
        ///   <code>head</code>,
        ///     <e:setminus>
        ///       <e:st n="S" />
        ///       <e:set><code>value</code></e:set>
        ///     </e:setminus>
        /// </e:pred>

        o = head;
        // Assignment.
        /// <e:pred name="List">
        ///   <code>o</code>,
        ///     <e:setminus>
        ///       <e:st n="S" />
        ///       <e:set><code>value</code></e:set>
        ///     </e:setminus>
        /// </e:pred>
      }
      // If-rule.
      /// <e:pred name="List">
      ///   <code>o</code>,
      ///     <e:setminus>
      ///       <e:st n="S" />
      ///       <e:set><code>value</code></e:set>
      ///     </e:setminus>
      /// </e:pred>
    }
    // If-rule.
    /// <e:pred name="List">
    ///   <code>o</code>,
    ///     <e:setminus>
    ///       <e:st n="S" />
    ///       <e:set><code>value</code></e:set>
    ///     </e:setminus>
    /// </e:pred>
  }
  // If-rule.
  /// <e:pred name="List">
  ///   <code>o</code>,
  ///     <e:setminus>
  ///       <e:st n="S" />
  ///       <e:set><code>value</code></e:set>
  ///     </e:setminus>
  /// </e:pred>

  return o;
}