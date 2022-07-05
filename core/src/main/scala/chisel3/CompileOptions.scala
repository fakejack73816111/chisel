// SPDX-License-Identifier: Apache-2.0

package chisel3

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait CompileOptions {
  // ==== EASY TO MIGRATE ====

  // Should the reset type of Module be a Bool or a Reset
  // 1) Set migrateInferModuleReset=true, fix errors by mixing in [[RequireSyncReset]])
  // 2) Set inferModuleReset=true
  val inferModuleReset: Boolean

  // When creating an object that takes a type argument, the argument must be unbound (a pure type).
  // 1) Set declaredTypeMustBeUnbound=true, fix all errors with calling chiselTypeOf on argument
  val declaredTypeMustBeUnbound: Boolean

  // Require explicit assignment of DontCare to generate "x is invalid"
  // 1) Discuss with Jack/Megan
  val explicitInvalidate: Boolean

  // ==== MEDIUM TO MIGRATE (currently) ====

  // Check that referenced Data have actually been declared.
  // 1) Set migrateCheckSynthesizable=true, and fix all errors
  // 2) Wait to actually change checkSynthesizable=true because it still currently affects connection semantics.. TODO!
  val checkSynthesizable: Boolean

  // ==== HARD TO MIGRATE (currently) ====

  // Should Record connections require a strict match of fields.
  // If true and the same fields aren't present in both source and sink, a MissingFieldException,
  // MissingLeftFieldException, or MissingRightFieldException will be thrown.
  val connectFieldsMustMatch: Boolean
  // If a connection operator fails, don't try the connection with the operands (source and sink) reversed.
  val dontTryConnectionsSwapped: Boolean
  // If connection directionality is not explicit, do not use heuristics to attempt to determine it.
  val dontAssumeDirectionality: Boolean

  // ==== Useful fields to help with migration ====

  /** If marked true, then any Module which consumes `inferModuleReset=false` must also mix in [[RequireSyncReset]] */
  def migrateInferModuleReset: Boolean = false

  /** If marked true, some APIs (e.g. <>, :=, dontCare) will require its argument to have been bound to a synthesizable hardware component. */
  // Note that this is necessary to only change this checking behavior, but not affect connection semantics due to its implementation
  def migrateCheckSynthesizable: Boolean = false
}

object CompileOptions {
  // Provides a low priority Strict default. Can be overridden by importing the NotStrict option.
  // Implemented as a macro to prevent this from being used inside chisel core.
  implicit def materialize: CompileOptions = macro materialize_impl

  def materialize_impl(c: Context): c.Tree = {
    import c.universe._
    q"_root_.chisel3.ExplicitCompileOptions.Strict"
  }
}

object ExplicitCompileOptions {
  case class CompileOptionsClass(
    // Should Record connections require a strict match of fields.
    // If true and the same fields aren't present in both source and sink, a MissingFieldException,
    // MissingLeftFieldException, or MissingRightFieldException will be thrown.
    val connectFieldsMustMatch: Boolean,
    // When creating an object that takes a type argument, the argument must be unbound (a pure type).
    val declaredTypeMustBeUnbound: Boolean,
    // If a connection operator fails, don't try the connection with the operands (source and sink) reversed.
    val dontTryConnectionsSwapped: Boolean,
    // If connection directionality is not explicit, do not use heuristics to attempt to determine it.
    val dontAssumeDirectionality: Boolean,
    // Check that referenced Data have actually been declared.
    val checkSynthesizable: Boolean,
    // Require an explicit DontCare assignment to generate a firrtl DefInvalid
    val explicitInvalidate: Boolean,
    // Should the reset type of Module be a Bool or a Reset
    val inferModuleReset: Boolean)
      extends CompileOptions

  // Collection of "not strict" connection compile options.
  // These provide compatibility with existing code.
  implicit val NotStrict = new CompileOptionsClass(
    connectFieldsMustMatch = false,
    declaredTypeMustBeUnbound = false,
    dontTryConnectionsSwapped = false,
    dontAssumeDirectionality = false,
    checkSynthesizable = false,
    explicitInvalidate = false,
    inferModuleReset = false
  )

  // Collection of "strict" connection compile options, preferred for new code.
  implicit val Strict = new CompileOptionsClass(
    connectFieldsMustMatch = true,
    declaredTypeMustBeUnbound = true,
    dontTryConnectionsSwapped = true,
    dontAssumeDirectionality = true,
    checkSynthesizable = true,
    explicitInvalidate = true,
    inferModuleReset = true
  )
}
