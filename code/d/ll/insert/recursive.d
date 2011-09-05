module ll.insert.recursive;

import ll.node;

Node* insert(Node* head, int value) {
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

    // Open <e:pred name="EmptyList"><code>head</code>, <e:st n="S"/></e:pred>.  Discard
    // <e:eq>
    //   <e:fst><code>head</code></e:fst>
    //   <e:snd><code>null</code></e:snd>
    // </e:eq>.
    /// <e:eq>
    ///   <e:fst><e:st n="S"/></e:fst>
    ///   <e:snd><m:scemp/></e:snd>
    /// </e:eq> ∧
    /// <m:hemp/>

    o = new Node(value);
    // Specification for <code>new Node(value)</code>.
    /// <e:eq>
    ///   <e:fst><e:st n="S"/></e:fst>
    ///   <e:snd><m:scemp/></e:snd>
    /// </e:eq> ∧
    /// <e:sep>
    ///   <e:fst><m:hemp/></e:fst>
    ///   <e:snd><e:pred name="NonEmptyList"><code>o</code>, <e:set><code>value</code></e:set></e:pred></e:snd>
    /// </e:sep>

    // <e:eq>
    //   <e:fst><e:sep>
    //     <e:fst><m:hemp/></e:fst>
    //     <e:snd><e:predicate>X</e:predicate></e:snd>
    //   </e:sep></e:fst>
    //   <e:snd><e:predicate>X</e:predicate></e:snd>
    // </e:eq>.
    /// <e:eq>
    ///   <e:fst><e:st n="S"/></e:fst>
    ///   <e:snd><m:scemp/></e:snd>
    /// </e:eq> ∧ <e:pred name="NonEmptyList"><code>o</code>, <e:set><code>value</code></e:set></e:pred>

    // <e:eq><e:fst><e:st n="X"/></e:fst><e:snd><e:union><e:fst><m:scemp/></e:fst><e:snd><e:st n="X"/></e:snd></e:union></e:snd></e:eq>.
    /// <e:eq>
    ///   <e:fst><e:st n="S"/></e:fst>
    ///   <e:snd><m:scemp/></e:snd>
    /// </e:eq> ∧ 
    /// <e:pred name="NonEmptyList"><code>o</code>,
    /// <e:union>
    ///   <e:fst><m:scemp/></e:fst>
    ///   <e:snd><e:set><code>value</code></e:set></e:snd>
    /// </e:union></e:pred>

    // Substitution.  Discard <e:eq><e:fst><e:st n="S"/></e:fst><e:snd><m:scemp/></e:snd></e:eq>.
    /// <e:pred name="NonEmptyList"><code>o</code>,
    /// <e:union>
    ///   <e:fst><e:st n="S"/></e:fst>
    ///   <e:snd><e:set><code>value</code></e:set></e:snd>
    /// </e:union></e:pred>
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
    ///   <e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
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
      ///   <e:pred name="Compose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
      ///   <e:sep>
      ///     <e:fst><e:fcell>
      ///       <e:fst><code>head</code></e:fst>
      ///       <e:snd><code>value</code>, <e:var n="tail"/></e:snd>
      ///     </e:fcell></e:fst>
      ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
      ///   </e:sep>
      /// </e:indent>

      // <e:logimpl>
      //   <e:fst><e:pred name="Compose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred></e:fst>
      //   <e:snd><e:in>
      //     <e:fst><code>value</code></e:fst>
      //     <e:snd><e:st n="S"/></e:snd>
      //   </e:in></e:snd>
      // </e:logimpl>.
      /// ∃<e:var n="tail"/>, <e:st n="T"/>.<br />
      /// <e:indent>
      ///   <e:pred name="Compose"><code>value</code>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
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

      // <e:logimpl>
      //   <e:fst><e:in>
      //     <e:fst><e:var n="a"/></e:fst>
      //     <e:snd><e:st n="B"/></e:snd>
      //   </e:in></e:fst>
      //   <e:snd><e:subset>
      //     <e:fst><e:set><e:var n="a"/></e:set></e:fst>
      //     <e:snd><e:st n="B"/></e:snd>
      //   </e:subset></e:snd>
      // </e:logimpl>
      /// <e:and>
      ///   <e:fst><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:subset>
      ///     <e:fst><e:set><code>value</code></e:set></e:fst>
      ///     <e:snd><e:st n="S"/></e:snd>
      ///   </e:subset></e:snd>
      /// </e:and>

      // <e:logimpl>
      //   <e:fst><e:subset>
      //     <e:fst><e:st n="A"/></e:fst>
      //     <e:snd><e:st n="B"/></e:snd>
      //   </e:subset></e:fst>
      //   <e:snd><e:eq>
      //     <e:fst><e:union>
      //       <e:fst><e:st n="B"/></e:fst>
      //       <e:snd><e:st n="A"/></e:snd>
      //     </e:union></e:fst>
      //     <e:snd><e:st n="B"/></e:snd>
      //   </e:eq></e:snd>
      // </e:logimpl>
      /// <e:and>
      ///   <e:fst><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:eq>
      ///     <e:fst><e:union>
      ///       <e:fst><e:set><code>value</code></e:set></e:fst>
      ///       <e:snd><e:st n="S" /></e:snd>
      ///     </e:union></e:fst>
      ///     <e:snd><e:st n="S"/></e:snd>
      ///   </e:eq></e:snd>
      /// </e:and>

      // Substitution.  Discard
      // <e:eq>
      //   <e:fst><e:union>
      //     <e:fst><e:set><code>value</code></e:set></e:fst>
      //     <e:snd><e:st n="S" /></e:snd>
      //   </e:union></e:fst>
      //   <e:snd><e:st n="S"/></e:snd>
      // </e:eq>.
      /// <e:fst><e:pred name="NonEmptyList">
      ///   <code>head</code>,
      ///     <e:union>
      ///       <e:fst><e:set><code>value</code></e:set></e:fst>
      ///       <e:snd><e:st n="S" /></e:snd>
      ///     </e:union>
      /// </e:pred></e:fst>

      o = head;
      // Assignment.
      /// <e:pred name="NonEmptyList">
      ///   <code>o</code>,
      ///     <e:union>
      ///       <e:fst><e:set><code>value</code></e:set></e:fst>
      ///       <e:snd><e:st n="S" /></e:snd>
      ///     </e:union>
      /// </e:pred>
    }
    else {
      // Deny if-condition.
      /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
      /// <e:indent>
      ///   <e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
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
        ///   <e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
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
        
        Node* ntail = insert(head.tail, value);
        // Use specification for <code>insert</code>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><code>ntail</code>, <e:union><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:fst><e:var n="v"/></e:fst>
        ///     <e:snd><code>value</code></e:snd>
        ///   </e:lt>
        /// </e:indent>

        head.tail = ntail;
        // Assignment.  Reintroduce existential quantification.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:union><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:fst><e:var n="v"/></e:fst>
        ///     <e:snd><code>value</code></e:snd>
        ///   </e:lt>
        /// </e:indent>

        // Lemma: <e:logimpl>
        //   <e:fst><e:and>
        //     <e:fst><e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred></e:fst>
        //     <e:snd><e:lt><e:fst><e:var n="v"/></e:fst><e:snd><code>value</code></e:snd></e:lt></e:snd>
        //   </e:and></e:fst>
        //   <e:snd><e:pred name="Compose">
        //     <e:var n="v"/>,
        //     <e:union><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>,
        //     <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>
        //   </e:pred></e:snd>
        // </e:logimpl>
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="Compose"><e:var n="v"/>, <e:union><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>head</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:union><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred></e:snd>
        ///   </e:sep>
        /// </e:indent>

        // Introduce existential quantification on <e:union><e:fst><e:st n="T"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred> ∧<br />
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
        //     <e:union>
        //       <e:fst><e:st n="S" /></e:fst>
        //       <e:snd><e:set><code>value</code></e:set></e:snd>
        //     </e:union>
        // </e:pred>.
        /// <e:pred name="NonEmptyList">
        ///   <code>head</code>,
        ///     <e:union>
        ///       <e:fst><e:st n="S" /></e:fst>
        ///       <e:snd><e:set><code>value</code></e:set></e:snd>
        ///     </e:union>
        /// </e:pred>

        o = head;
        // Assignment.
        /// <e:pred name="NonEmptyList">
        ///   <code>o</code>,
        ///     <e:union>
        ///       <e:fst><e:st n="S" /></e:fst>
        ///       <e:snd><e:set><code>value</code></e:set></e:snd>
        ///     </e:union>
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
        ///   <e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
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
        ///   <e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
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

        nhead = new Node(value, head);
        // Specification for <code>new Node(value, head)</code>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>nhead</code></e:fst>
        ///       <e:snd><code>value</code>, <code>head</code></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:sep>
        ///       <e:fst><e:fcell>
        ///         <e:fst><code>head</code></e:fst>
        ///         <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///       </e:fcell></e:fst>
        ///       <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
        ///     </e:sep></e:snd>
        ///   </e:sep> ∧<br />
        ///   <e:lt>
        ///     <e:fst><code>value</code></e:fst>
        ///     <e:snd><e:var n="v"/></e:snd>
        ///   </e:lt>
        /// </e:indent>

        // Close <e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred>.
        /// <e:and>
        ///   <e:fst><e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred></e:fst>
        ///   <e:snd><e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt></e:snd>
        /// </e:and> ∧<br />
        /// <e:sep>
        ///   <e:fst><e:fcell>
        ///     <e:fst><code>nhead</code></e:fst>
        ///     <e:snd><code>value</code>, <code>head</code></e:snd>
        ///   </e:fcell></e:fst>
        ///   <e:snd><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
        /// </e:sep>

        // Lemma: <e:logimpl>
        //  <e:fst><e:and>
        //    <e:fst><e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:st n="S"/></e:pred></e:fst>
        //    <e:snd><e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt></e:snd>
        //  </e:and></e:fst>
        //  <e:snd><e:pred name="Compose"><code>value</code>, <e:st n="S"/>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred></e:snd>
        // </e:logimpl>
        /// <e:pred name="Compose"><code>value</code>, <e:st n="S"/>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred> ∧<br />
        /// <e:sep>
        ///   <e:fst><e:fcell>
        ///     <e:fst><code>nhead</code></e:fst>
        ///     <e:snd><code>value</code>, <code>head</code></e:snd>
        ///   </e:fcell></e:fst>
        ///   <e:snd><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
        /// </e:sep>

        // Weakening lemma: <e:logimpl>
        //   <e:fst><e:pred name="NonEmptyList"><code>head</code>, <e:st n="S"/></e:pred></e:fst>
        //   <e:snd><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
        // </e:logimpl>.
        /// <e:pred name="Compose"><code>value</code>, <e:st n="S"/>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred> ∧<br />
        /// <e:sep>
        ///   <e:fst><e:fcell>
        ///     <e:fst><code>nhead</code></e:fst>
        ///     <e:snd><code>value</code>, <code>head</code></e:snd>
        ///   </e:fcell></e:fst>
        ///   <e:snd><e:pred name="List"><code>head</code>, <e:st n="S"/></e:pred></e:snd>
        /// </e:sep>

        // <m:existsIntro/> on <e:st n="S"/> as <e:st n="T"/>, <code>head</code> as <e:var n="tail"/>, and <code>value</code> as <e:var n="v"/>.
        /// ∃<e:var n="v"/>, <e:var n="tail"/>, <e:st n="T"/>.<br />
        /// <e:indent>
        ///   <e:pred name="Compose"><e:var n="v"/>, <e:st n="T"/>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred> ∧<br />
        ///   <e:sep>
        ///     <e:fst><e:fcell>
        ///       <e:fst><code>nhead</code></e:fst>
        ///       <e:snd><e:var n="v"/>, <e:var n="tail"/></e:snd>
        ///     </e:fcell></e:fst>
        ///     <e:snd><e:pred name="List"><e:var n="tail"/>, <e:st n="T"/></e:pred></e:snd>
        ///   </e:sep>
        /// </e:indent>

        // Close <e:pred name="NonEmptyList">
        //   <code>nhead</code>,
        //     <e:union>
        //       <e:fst><e:st n="S" /></e:fst>
        //       <e:snd><e:set><code>value</code></e:set></e:snd>
        //     </e:union>
        // </e:pred>.
        /// <e:pred name="NonEmptyList">
        ///   <code>nhead</code>,
        ///     <e:union>
        ///       <e:fst><e:st n="S" /></e:fst>
        ///       <e:snd><e:set><code>value</code></e:set></e:snd>
        ///     </e:union>
        /// </e:pred>

        o = nhead;
        // Assignment.
        /// <e:pred name="NonEmptyList">
        ///   <code>o</code>,
        ///     <e:union>
        ///       <e:fst><e:st n="S" /></e:fst>
        ///       <e:snd><e:set><code>value</code></e:set></e:snd>
        ///     </e:union>
        /// </e:pred>
      }
      // If-rule.
      /// <e:pred name="NonEmptyList">
      ///   <code>o</code>,
      ///     <e:union>
      ///       <e:fst><e:st n="S" /></e:fst>
      ///       <e:snd><e:set><code>value</code></e:set></e:snd>
      ///     </e:union>
      /// </e:pred>
    }
    // If-rule.
    /// <e:pred name="NonEmptyList">
    ///   <code>o</code>,
    ///     <e:union>
    ///       <e:fst><e:st n="S" /></e:fst>
    ///       <e:snd><e:set><code>value</code></e:set></e:snd>
    ///     </e:union>
    /// </e:pred>
  }
  // If-rule.
  /// <e:pred name="NonEmptyList">
  ///   <code>o</code>,
  ///     <e:union>
  ///       <e:fst><e:st n="S" /></e:fst>
  ///       <e:snd><e:set><code>value</code></e:set></e:snd>
  ///     </e:union>
  /// </e:pred>
  return o;
}