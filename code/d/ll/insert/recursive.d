module ll.insert.recursive;

import ll.node;

Node* insert(Node* head, int value) {
  Node* o;
  // Precondition.
  /// <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>

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
    //     <e:eq>
    //       <code>head</code>
    //       <m:null/>
    //     </e:eq>
    //   </e:and>
    //   <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>
    // </e:logimpl>
    /// <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>

    // Open <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>.  Discard
    // <e:eq>
    //   <code>head</code>
    //   <m:null/>
    // </e:eq>.
    /// <e:eq>
    ///   <e:st n="S"/>
    ///   <m:empty/>
    /// </e:eq> ∧
    /// <m:hemp/>

    o = new Node(value);
    // Specification for <code>new Node(value)</code>.
    /// <e:eq>
    ///   <e:st n="S"/>
    ///   <m:empty/>
    /// </e:eq> ∧
    /// <e:sep>
    ///   <m:hemp/>
    ///   <e:pred name="NonEmptyList"><code>o</code>, <e:set><code>value</code></e:set></e:pred>
    /// </e:sep>

    // <e:eq>
    //   <e:sep>
    //     <m:hemp/>
    //     <e:predicate>X</e:predicate>
    //   </e:sep>
    //   <e:predicate>X</e:predicate>
    // </e:eq>.
    /// <e:eq>
    ///   <e:st n="S"/>
    ///   <m:empty/>
    /// </e:eq> ∧ <e:pred name="NonEmptyList"><code>o</code>, <e:set><code>value</code></e:set></e:pred>

    // <e:eq><e:st n="X"/><e:union><m:empty/><e:st n="X"/></e:union></e:eq>.
    /// <e:eq>
    ///   <e:st n="S"/>
    ///   <m:empty/>
    /// </e:eq> ∧ 
    /// <e:pred name="NonEmptyList"><code>o</code>,
    /// <e:union>
    ///   <m:empty/>
    ///   <e:set><code>value</code></e:set>
    /// </e:union></e:pred>

    // Substitution.  Discard <e:eq><e:st n="S"/><m:empty/></e:eq>.
    /// <e:pred name="NonEmptyList"><code>o</code>,
    /// <e:union>
    ///   <e:st n="S"/>
    ///   <e:set><code>value</code></e:set>
    /// </e:union></e:pred>
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

      // <e:logimpl>
      //   <e:in>
      //     <e:var n="a"/>
      //     <e:st n="B"/>
      //   </e:in>
      //   <e:subset>
      //     <e:set><e:var n="a"/></e:set>
      //     <e:st n="B"/>
      //   </e:subset>
      // </e:logimpl>
      /// <e:and>
      ///   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
      ///   <e:subset>
      ///     <e:set><code>value</code></e:set>
      ///     <e:st n="S"/>
      ///   </e:subset>
      /// </e:and>

      // <e:logimpl>
      //   <e:subset>
      //     <e:st n="A"/>
      //     <e:st n="B"/>
      //   </e:subset>
      //   <e:eq>
      //     <e:union>
      //       <e:st n="B"/>
      //       <e:st n="A"/>
      //     </e:union>
      //     <e:st n="B"/>
      //   </e:eq>
      // </e:logimpl>
      /// <e:and>
      ///   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
      ///   <e:eq>
      ///     <e:union>
      ///       <e:set><code>value</code></e:set>
      ///       <e:st n="S" />
      ///     </e:union>
      ///     <e:st n="S"/>
      ///   </e:eq>
      /// </e:and>

      // Substitution.  Discard
      // <e:eq>
      //   <e:union>
      //     <e:set><code>value</code></e:set>
      //     <e:st n="S" />
      //   </e:union>
      //   <e:st n="S"/>
      // </e:eq>.
      /// <e:pred name="NonEmptyList">
      ///   <code>head</code>,
      ///     <e:union>
      ///       <e:set><code>value</code></e:set>
      ///       <e:st n="S" />
      ///     </e:union>
      /// </e:pred>

      o = head;
      // Assignment.
      /// <e:pred name="NonEmptyList">
      ///   <code>o</code>,
      ///     <e:union>
      ///       <e:set><code>value</code></e:set>
      ///       <e:st n="S" />
      ///     </e:union>
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
        
        Node* ntail = insert(head.tail, value);
        // Use specification for <code>insert</code>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><code>ntail</code>, <e:union><e:st n="T"/><e:set><code>value</code></e:set></e:union></e:pred>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:var n="v"/>
        ///     <code>value</code>
        ///   </e:lt>
        /// </e:indent>

        head.tail = ntail;
        // Assignment.  Reintroduce existential quantification.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:union><e:st n="T"/><e:set><code>value</code></e:set></e:union></e:pred>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:var n="v"/>
        ///     <code>value</code>
        ///   </e:lt>
        /// </e:indent>

        // Lemma: <e:logimpl>
        //   <e:and>
        //     <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred>
        //     <e:lt><e:var n="v"/><code>value</code></e:lt>
        //   </e:and>
        //   <e:pred name="ListCompose">
        //     <e:var n="v"/>,
        //     <e:union><e:st n="T"/><e:set><code>value</code></e:set></e:union>,
        //     <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union>
        //   </e:pred>
        // </e:logimpl>
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:union><e:st n="T"/><e:set><code>value</code></e:set></e:union>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>head</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:union><e:st n="T"/><e:set><code>value</code></e:set></e:union></e:pred>
        ///   </e:sep>
        /// </e:indent>

        // Introduce existential quantification on <e:union><e:st n="T"/><e:set><code>value</code></e:set></e:union>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union></e:pred> ∧<br />
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
        //     <e:union>
        //       <e:st n="S" />
        //       <e:set><code>value</code></e:set>
        //     </e:union>
        // </e:pred>.
        /// <e:pred name="NonEmptyList">
        ///   <code>head</code>,
        ///     <e:union>
        ///       <e:st n="S" />
        ///       <e:set><code>value</code></e:set>
        ///     </e:union>
        /// </e:pred>

        o = head;
        // Assignment.
        /// <e:pred name="NonEmptyList">
        ///   <code>o</code>,
        ///     <e:union>
        ///       <e:st n="S" />
        ///       <e:set><code>value</code></e:set>
        ///     </e:union>
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

        nhead = new Node(value, head);
        // Specification for <code>new Node(value, head)</code>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>nhead</code>
        ///       <e:list><code>value</code>, <code>head</code></e:list>
        ///     </e:fcell>
        ///     <e:sep>
        ///       <e:fcell>
        ///         <code>head</code>
        ///         <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///       </e:fcell>
        ///       <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
        ///     </e:sep>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <code>value</code>
        ///     <e:var n="v"/>
        ///   </e:lt>
        /// </e:indent>

        // Close <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>.
        /// <e:and>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred>
        ///   <e:lt><code>value</code><e:var n="v"/></e:lt>
        /// </e:and> ∧<br />
        /// <e:sep>
        ///   <e:fcell>
        ///     <code>nhead</code>
        ///     <e:list><code>value</code>, <code>head</code></e:list>
        ///   </e:fcell>
        ///   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
        /// </e:sep>

        // Lemma: <e:logimpl>
        //  <e:and>
        //    <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred>
        //    <e:lt><code>value</code><e:var n="v"/></e:lt>
        //  </e:and>
        //  <e:pred name="ListCompose"><code>value</code>, <e:st n="S"/>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union></e:pred>
        // </e:logimpl>
        /// <e:pred name="ListCompose"><code>value</code>, <e:st n="S"/>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union></e:pred> ∧<br />
        /// <e:sep>
        ///   <e:fcell>
        ///     <code>nhead</code>
        ///     <e:list><code>value</code>, <code>head</code></e:list>
        ///   </e:fcell>
        ///   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
        /// </e:sep>

        // Weakening lemma: <e:logimpl>
        //   <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>
        //   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
        // </e:logimpl>.
        /// <e:pred name="ListCompose"><code>value</code>, <e:st n="S"/>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union></e:pred> ∧<br />
        /// <e:sep>
        ///   <e:fcell>
        ///     <code>nhead</code>
        ///     <e:list><code>value</code>, <code>head</code></e:list>
        ///   </e:fcell>
        ///   <e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred>
        /// </e:sep>

        // <m:existsIntro/> on <e:st n="S"/> as <e:st n="T"/>, <code>head</code> as <e:var n="tail"/>, and <code>value</code> as <e:var n="v"/>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="ListCompose"><e:var n="v"/>, <e:st n="T"/>, <e:union><e:st n="S"/><e:set><code>value</code></e:set></e:union></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fcell>
        ///       <code>nhead</code>
        ///       <e:list><e:var n="v"/>, <e:var n="tail"/></e:list>
        ///     </e:fcell>
        ///     <e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred>
        ///   </e:sep>
        /// </e:indent>

        // Close <e:pred name="NonEmptyList">
        //   <code>nhead</code>,
        //     <e:union>
        //       <e:st n="S" />
        //       <e:set><code>value</code></e:set>
        //     </e:union>
        // </e:pred>.
        /// <e:pred name="NonEmptyList">
        ///   <code>nhead</code>,
        ///     <e:union>
        ///       <e:st n="S" />
        ///       <e:set><code>value</code></e:set>
        ///     </e:union>
        /// </e:pred>

        o = nhead;
        // Assignment.
        /// <e:pred name="NonEmptyList">
        ///   <code>o</code>,
        ///     <e:union>
        ///       <e:st n="S" />
        ///       <e:set><code>value</code></e:set>
        ///     </e:union>
        /// </e:pred>
      }
      // If-rule.
      /// <e:pred name="NonEmptyList">
      ///   <code>o</code>,
      ///     <e:union>
      ///       <e:st n="S" />
      ///       <e:set><code>value</code></e:set>
      ///     </e:union>
      /// </e:pred>
    }
    // If-rule.
    /// <e:pred name="NonEmptyList">
    ///   <code>o</code>,
    ///     <e:union>
    ///       <e:st n="S" />
    ///       <e:set><code>value</code></e:set>
    ///     </e:union>
    /// </e:pred>
  }
  // If-rule.
  /// <e:pred name="NonEmptyList">
  ///   <code>o</code>,
  ///     <e:union>
  ///       <e:st n="S" />
  ///       <e:set><code>value</code></e:set>
  ///     </e:union>
  /// </e:pred>
  return o;
}