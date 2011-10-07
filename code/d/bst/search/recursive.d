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
    ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
    ///   <e:eq><code>root</code><code>null</code></e:eq>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:and>
    //     <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
    //     <e:eq><code>root</code><code>null</code></e:eq>
    //   </e:and>
    //   <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    // </e:logimpl>
    /// <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>

    // <e:logimpl>
    //   <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    //   <e:eq><e:st n="S"/><m:scemp/></e:eq>
    // </e:logimpl>
    /// <e:and>
    ///   <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    ///   <e:eq><e:st n="S"/><m:scemp/></e:eq>
    /// </e:and>

    // <e:logimpl>
    //   <e:eq><e:st n="S"/><m:scemp/></e:eq>
    //   <e:notin><code>value</code><e:st n="S"/></e:notin>
    // </e:logimpl>
    /// <e:and>
    ///   <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    ///   <e:notin><code>value</code><e:st n="S"/></e:notin>
    /// </e:and>

    o = false;
    // Assignment.
    /// <e:and>
    ///   <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    ///   <e:and>
    ///     <e:notin><code>value</code><e:st n="S"/></e:notin>
    ///     <e:eq><code>o</code><code>false</code></e:eq>
    ///   </e:and>
    /// </e:and>

    // ?
    /// <e:and>
    ///   <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    ///   <e:doubleimpl>
    ///     <code>o</code>
    ///     (<e:in><code>value</code><e:st n="S"/></e:in>)
    ///   </e:doubleimpl>
    /// </e:and>

    // Weakening lemma: <e:logimpl>
    //   <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    //   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
    // </e:logimpl>
    /// <e:and>
    ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
    ///   <e:doubleimpl>
    ///     <code>o</code>
    ///     (<e:in><code>value</code><e:st n="S"/></e:in>)
    ///   </e:doubleimpl>
    /// </e:and>
  }
  else {
    // Deny if-condition.
    /// <e:and>
    ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
    ///   <e:noteq><code>root</code><code>null</code></e:noteq>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:and>
    //     <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
    //     <e:noteq><code>root</code><code>null</code></e:noteq>
    //   </e:and>
    //   <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    // </e:logimpl>
    /// <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>

    bool eq = rootEq(root, value);
    // Specification for <code>rootEq</code>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/></e:vars>
    ///   <e:expr><e:and>
    ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
    ///     <e:doubleimpl>
    ///       <code>eq</code>
    ///       <e:eq>
    ///         <e:var n="v"/>
    ///         <code>value</code>
    ///       </e:eq>
    ///     </e:doubleimpl>
    ///   </e:and></e:expr>
    /// </e:exists>

    if (eq) {
      // Assert if-test.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/></e:vars>
      ///   <e:expr><e:and>
      ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
      ///     <e:doubleimpl>
      ///       <code>eq</code>
      ///       <e:eq>
      ///         <e:var n="v"/>
      ///         <code>value</code>
      ///       </e:eq>
      ///     </e:doubleimpl>
      ///   </e:and></e:expr>
      /// </e:exists> ∧ <code>eq</code>

      // Use <e:doubleimpl>
      //   <code>eq</code>
      //   <e:eq>
      //     <e:var n="v"/>
      //     <code>value</code>
      //   </e:eq>
      // </e:doubleimpl>.  Discard <code>eq</code>.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/></e:vars>
      ///   <e:expr><e:and>
      ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
      ///     <e:eq>
      ///         <e:var n="v"/>
      ///         <code>value</code>
      ///       </e:eq>
      ///   </e:and></e:expr>
      /// </e:exists>

      // Substitution.
      /// <e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred>

      // Lemma: <e:logimpl>
      //   <e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred>
      //   <e:in><code>value</code><e:st n="S"/></e:in>
      // </e:logimpl>
      /// <e:and>
      ///   <e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred>
      ///   <e:in><code>value</code><e:st n="S"/></e:in>
      /// </e:and>

      // Reintroduce <m:existsIntro/> on <code>value</code> as <e:var n="v"/>.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/></e:vars>
      ///   <e:expr><e:and>
      ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
      ///     <e:in><code>value</code><e:st n="S"/></e:in>
      ///   </e:and></e:expr>
      /// </e:exists>

      // Close <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>.
      /// <e:and>
      ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
      ///   <e:in><code>value</code><e:st n="S"/></e:in>
      /// </e:and>

      o = true;
      // Assignment.
      /// <e:and>
      ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
      ///   <e:in><code>value</code><e:st n="S"/></e:in>
      /// </e:and> ∧ <e:eq><code>o</code><code>true</code></e:eq>

      // ?
      /// <e:and>
      ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
      ///   <e:doubleimpl>
      ///     <code>o</code>
      ///     <e:in><code>value</code><e:st n="S"/></e:in>
      ///   </e:doubleimpl>
      /// </e:and>
    }
    else {
      // Deny if-condition.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/></e:vars>
      ///   <e:expr><e:and>
      ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
      ///     <e:doubleimpl>
      ///       <code>eq</code>
      ///       <e:eq>
      ///         <e:var n="v"/>
      ///         <code>value</code>
      ///       </e:eq>
      ///     </e:doubleimpl>
      ///   </e:and></e:expr>
      /// </e:exists> ∧ ¬<code>eq</code>

      // Use <e:impl>
      //   <code>eq</code>
      //   (<e:eq>
      //     <e:var n="v"/>
      //     <code>value</code>
      //   </e:eq>)
      // </e:impl>.  Discard ¬<code>eq</code>.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/></e:vars>
      ///   <e:expr><e:and>
      ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
      ///     <e:noteq>
      ///         <e:var n="v"/>
      ///         <code>value</code>
      ///       </e:noteq>
      ///   </e:and></e:expr>
      /// </e:exists>

      // Open <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
      ///   <e:expr><e:indent>
      ///     <e:sep>
      ///       <e:fcell>
      ///         <code>root</code>
      ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
      ///       </e:fcell>
      ///       <e:sep>
      ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
      ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
      ///       </e:sep>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
      ///     <e:noteq>
      ///       <e:var n="v"/>
      ///       <code>value</code>
      ///     </e:noteq>
      ///   </e:indent></e:expr>
      /// </e:exists>

      if (value < root.value) {
        // Assert if-condition.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///         <code>root</code>
        ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
        ///       </e:fcell>
        ///       <e:sep>
        ///           <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
        ///           <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:lt>
        ///       <code>value</code>
        ///       <e:var n="v"/>
        ///     </e:lt>
        ///   </e:indent></e:expr>
        /// </e:exists>

        // Lemma: <e:logimpl>
        //   <e:and>
        //     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
        //     <e:lt><code>value</code><e:var n="v"/></e:lt>
        //   </e:and>
        //   <e:doubleimpl>
        //     <e:in><code>value</code><e:st n="L"/></e:in>
        //     <e:in><code>value</code><e:st n="S"/></e:in>
        //   </e:doubleimpl>
        // </e:logimpl>.  Discard <e:lt><code>value</code><e:var n="v"/></e:lt>.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///           <code>root</code>
        ///           <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
        ///         </e:fcell>
        ///       <e:sep>
        ///           <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
        ///           <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:doubleimpl>
        ///       <e:in><code>value</code><e:st n="L"/></e:in>
        ///       <e:in><code>value</code><e:st n="S"/></e:in>
        ///     </e:doubleimpl>
        ///   </e:indent></e:expr>
        /// </e:exists>

        o = search(root.left, value);
        // Use specification for <code>search</code>.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///           <code>root</code>
        ///           <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
        ///         </e:fcell>
        ///       <e:sep>
        ///           <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
        ///           <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:doubleimpl>
        ///       <e:in><code>value</code><e:st n="L"/></e:in>
        ///       <e:in><code>value</code><e:st n="S"/></e:in>
        ///     </e:doubleimpl> ∧<br />
        ///     <e:doubleimpl>
        ///       <code>o</code>
        ///       (<e:in><code>value</code><e:st n="L"/></e:in>)
        ///     </e:doubleimpl>
        ///   </e:indent></e:expr>
        /// </e:exists>

        // Transitivity of double implication.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///           <code>root</code>
        ///           <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
        ///         </e:fcell>
        ///       <e:sep>
        ///           <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
        ///           <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:doubleimpl>
        ///       <code>o</code>
        ///       (<e:in><code>value</code><e:st n="S"/></e:in>)
        ///     </e:doubleimpl>
        ///   </e:indent></e:expr>
        /// </e:exists>

        // Close <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred> ∧
        ///     <e:doubleimpl>
        ///       <code>o</code>
        ///       (<e:in><code>value</code><e:st n="S"/></e:in>)
        ///     </e:doubleimpl>
        ///   </e:indent></e:expr>
        /// </e:exists>

        // Close <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>.
        /// <e:and>
        ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
        ///   <e:doubleimpl>
        ///     <code>o</code>
        ///     (<e:in><code>value</code><e:st n="S"/></e:in>)
        ///   </e:doubleimpl>
        /// </e:and>
      }
      else {
        // (Symmetrical case…)
        o = search(root.right, value);
        /// <e:and>
        ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
        ///   <e:doubleimpl>
        ///     <code>o</code>
        ///     (<e:in><code>value</code><e:st n="S"/></e:in>)
        ///   </e:doubleimpl>
        /// </e:and>
      }
      // If-rule.
      /// <e:and>
      ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
      ///   <e:doubleimpl>
      ///     <code>o</code>
      ///     (<e:in><code>value</code><e:st n="S"/></e:in>)
      ///   </e:doubleimpl>
      /// </e:and>
    }
    // If-rule.
    /// <e:and>
    ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
    ///   <e:doubleimpl>
    ///     <code>o</code>
    ///     (<e:in><code>value</code><e:st n="S"/></e:in>)
    ///   </e:doubleimpl>
    /// </e:and>
  }
  // If-rule. Function postcondition.
  /// <e:and>
  ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
  ///   <e:doubleimpl>
  ///     <code>o</code>
  ///     (<e:in><code>value</code><e:st n="S"/></e:in>)
  ///   </e:doubleimpl>
  /// </e:and>
  return o;
}
