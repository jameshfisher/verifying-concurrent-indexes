module bst.search.recursive;

import bst.node;
import bst.descend;


bool search(Node* root, in int value) {
  bool o;
  // Function precondition.
  /// <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>

  if (root == null) {
    // Assert if-condition.
    /// <e:and>
    ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:eq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:eq></e:snd>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:fst><e:and>
    //     <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    //     <e:snd><e:eq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:eq></e:snd>
    //   </e:and></e:fst>
    //   <e:snd><e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:snd>
    // </e:logimpl>
    /// <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>

    // <e:logimpl>
    //   <e:fst><e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    //   <e:snd><e:eq><e:fst><e:st n="S"/></e:fst><e:snd><m:scemp/></e:snd></e:eq></e:snd>
    // </e:logimpl>
    /// <e:and>
    ///   <e:fst><e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:eq><e:fst><e:st n="S"/></e:fst><e:snd><m:scemp/></e:snd></e:eq></e:snd>
    /// </e:and>

    // <e:logimpl>
    //   <e:fst><e:eq><e:fst><e:st n="S"/></e:fst><e:snd><m:scemp/></e:snd></e:eq></e:fst>
    //   <e:snd><e:notin><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:notin></e:snd>
    // </e:logimpl>
    /// <e:and>
    ///   <e:fst><e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:notin><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:notin></e:snd>
    /// </e:and>

    o = false;
    // Assignment.
    /// <e:and>
    ///   <e:fst><e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:and>
    ///     <e:fst><e:notin><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:notin></e:fst>
    ///     <e:snd><e:eq><e:fst><code>o</code></e:fst><e:snd><code>false</code></e:snd></e:eq></e:snd>
    ///   </e:and></e:snd>
    /// </e:and>

    // ?
    /// <e:and>
    ///   <e:fst><e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:doubleimpl>
    ///     <e:fst><code>o</code></e:fst>
    ///     <e:snd>(<e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in>)</e:snd>
    ///   </e:doubleimpl></e:snd>
    /// </e:and>

    // Weakening lemma: <e:logimpl>
    //   <e:fst><e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    //   <e:snd><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:snd>
    // </e:logimpl>
    /// <e:and>
    ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:doubleimpl>
    ///     <e:fst><code>o</code></e:fst>
    ///     <e:snd>(<e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in>)</e:snd>
    ///   </e:doubleimpl></e:snd>
    /// </e:and>
  }
  else {
    // Deny if-condition.
    /// <e:and>
    ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:noteq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:noteq></e:snd>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:fst><e:and>
    //     <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    //     <e:snd><e:noteq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:noteq></e:snd>
    //   </e:and></e:fst>
    //   <e:snd><e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:snd>
    // </e:logimpl>
    /// <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>

    bool eq = rootEq(root, value);
    // Specification for <code>rootEq</code>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/></e:fst>
    ///   <e:snd><e:and>
    ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
    ///     <e:snd><e:doubleimpl>
    ///       <e:fst><code>eq</code></e:fst>
    ///       <e:snd><e:eq>
    ///         <e:fst><e:var n="v"/></e:fst>
    ///         <e:snd><code>value</code></e:snd>
    ///       </e:eq></e:snd>
    ///     </e:doubleimpl></e:snd>
    ///   </e:and></e:snd>
    /// </e:exists>

    if (eq) {
      // Assert if-test.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:doubleimpl>
      ///       <e:fst><code>eq</code></e:fst>
      ///       <e:snd><e:eq>
      ///         <e:fst><e:var n="v"/></e:fst>
      ///         <e:snd><code>value</code></e:snd>
      ///       </e:eq></e:snd>
      ///     </e:doubleimpl></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists> ∧ <code>eq</code>

      // Use <e:doubleimpl>
      //   <e:fst><code>eq</code></e:fst>
      //   <e:snd><e:eq>
      //     <e:fst><e:var n="v"/></e:fst>
      //     <e:snd><code>value</code></e:snd>
      //   </e:eq></e:snd>
      // </e:doubleimpl>.  Discard <code>eq</code>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:eq>
      ///         <e:fst><e:var n="v"/></e:fst>
      ///         <e:snd><code>value</code></e:snd>
      ///       </e:eq></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists>

      // Substitution.
      /// <e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred>

      // Lemma: <e:logimpl>
      //   <e:fst><e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred></e:fst>
      //   <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
      // </e:logimpl>
      /// <e:and>
      ///   <e:fst><e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
      /// </e:and>

      // Reintroduce <m:existsIntro/> on <code>value</code> as <e:var n="v"/>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists>

      // Close <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>.
      /// <e:and>
      ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
      /// </e:and>

      o = true;
      // Assignment.
      /// <e:and>
      ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
      /// </e:and> ∧ <e:eq><e:fst><code>o</code></e:fst><e:snd><code>true</code></e:snd></e:eq>

      // ?
      /// <e:and>
      ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:doubleimpl>
      ///     <e:fst><code>o</code></e:fst>
      ///     <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
      ///   </e:doubleimpl></e:snd>
      /// </e:and>
    }
    else {
      // Deny if-condition.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:doubleimpl>
      ///       <e:fst><code>eq</code></e:fst>
      ///       <e:snd><e:eq>
      ///         <e:fst><e:var n="v"/></e:fst>
      ///         <e:snd><code>value</code></e:snd>
      ///       </e:eq></e:snd>
      ///     </e:doubleimpl></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists> ∧ ¬<code>eq</code>

      // Use <e:impl>
      //   <e:fst><code>eq</code></e:fst>
      //   <e:snd>(<e:eq>
      //     <e:fst><e:var n="v"/></e:fst>
      //     <e:snd><code>value</code></e:snd>
      //   </e:eq>)</e:snd>
      // </e:impl>.  Discard ¬<code>eq</code>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:noteq>
      ///         <e:fst><e:var n="v"/></e:fst>
      ///         <e:snd><code>value</code></e:snd>
      ///       </e:noteq></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists>

      // Open <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
      ///   <e:snd><e:indent>
      ///     <e:sep>
      ///       <e:fst><e:fcell>
      ///             <e:fst><code>root</code></e:fst>
      ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
      ///           </e:fcell></e:fst>
      ///       <e:snd><e:sep>
      ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
      ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
      ///       </e:sep></e:snd>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
      ///     <e:noteq>
      ///       <e:fst><e:var n="v"/></e:fst>
      ///       <e:snd><code>value</code></e:snd>
      ///     </e:noteq>
      ///   </e:indent></e:snd>
      /// </e:exists>

      if (value < root.value) {
        // Assert if-condition.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///           <e:fst><code>root</code></e:fst>
        ///           <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
        ///         </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///           <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
        ///           <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:lt>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:var n="v"/></e:snd>
        ///     </e:lt>
        ///   </e:indent></e:snd>
        /// </e:exists>

        // Lemma: <e:logimpl>
        //   <e:fst><e:and>
        //     <e:fst><e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:fst>
        //     <e:snd><e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt></e:snd>
        //   </e:and></e:fst>
        //   <e:snd><e:doubleimpl>
        //     <e:fst><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="L"/></e:snd></e:in></e:fst>
        //     <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
        //   </e:doubleimpl></e:snd>
        // </e:logimpl>.  Discard <e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt>.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///           <e:fst><code>root</code></e:fst>
        ///           <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
        ///         </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///           <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
        ///           <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:doubleimpl>
        ///       <e:fst><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="L"/></e:snd></e:in></e:fst>
        ///       <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
        ///     </e:doubleimpl>
        ///   </e:indent></e:snd>
        /// </e:exists>

        o = search(root.left, value);
        // Use specification for <code>search</code>.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///           <e:fst><code>root</code></e:fst>
        ///           <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
        ///         </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///           <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
        ///           <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:doubleimpl>
        ///       <e:fst><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="L"/></e:snd></e:in></e:fst>
        ///       <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
        ///     </e:doubleimpl> ∧<br />
        ///     <e:doubleimpl>
        ///       <e:fst><code>o</code></e:fst>
        ///       <e:snd>(<e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="L"/></e:snd></e:in>)</e:snd>
        ///     </e:doubleimpl>
        ///   </e:indent></e:snd>
        /// </e:exists>

        // Transitivity of double implication.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///           <e:fst><code>root</code></e:fst>
        ///           <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
        ///         </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///           <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
        ///           <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:doubleimpl>
        ///       <e:fst><code>o</code></e:fst>
        ///       <e:snd>(<e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in>)</e:snd>
        ///     </e:doubleimpl>
        ///   </e:indent></e:snd>
        /// </e:exists>

        // Close <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred> ∧
        ///     <e:doubleimpl>
        ///       <e:fst><code>o</code></e:fst>
        ///       <e:snd>(<e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in>)</e:snd>
        ///     </e:doubleimpl>
        ///   </e:indent></e:snd>
        /// </e:exists>

        // Close <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>.
        /// <e:and>
        ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
        ///   <e:snd><e:doubleimpl>
        ///     <e:fst><code>o</code></e:fst>
        ///     <e:snd>(<e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in>)</e:snd>
        ///   </e:doubleimpl></e:snd>
        /// </e:and>
      }
      else {
        // (Symmetrical case…)
        o = search(root.right, value);
        /// <e:and>
        ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
        ///   <e:snd><e:doubleimpl>
        ///     <e:fst><code>o</code></e:fst>
        ///     <e:snd>(<e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in>)</e:snd>
        ///   </e:doubleimpl></e:snd>
        /// </e:and>
      }
      // If-rule.
      /// <e:and>
      ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:doubleimpl>
      ///     <e:fst><code>o</code></e:fst>
      ///     <e:snd>(<e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in>)</e:snd>
      ///   </e:doubleimpl></e:snd>
      /// </e:and>
    }
    // If-rule.
    /// <e:and>
    ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:doubleimpl>
    ///     <e:fst><code>o</code></e:fst>
    ///     <e:snd>(<e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in>)</e:snd>
    ///   </e:doubleimpl></e:snd>
    /// </e:and>
  }
  // If-rule. Function postcondition.
  /// <e:and>
  ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
  ///   <e:snd><e:doubleimpl>
  ///     <e:fst><code>o</code></e:fst>
  ///     <e:snd>(<e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in>)</e:snd>
  ///   </e:doubleimpl></e:snd>
  /// </e:and>
  return o;
}
