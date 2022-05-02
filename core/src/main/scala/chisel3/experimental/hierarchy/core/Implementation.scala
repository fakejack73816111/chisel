// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental.hierarchy.core
import scala.collection.mutable.HashMap
import scala.reflect.runtime.universe.TypeTag
import scala.language.experimental.macros
import java.util.IdentityHashMap

sealed trait Implementation {
  type P
  def implement(d: ResolvedDefinition[P]): Unit
}

trait CustomImplementation extends Implementation