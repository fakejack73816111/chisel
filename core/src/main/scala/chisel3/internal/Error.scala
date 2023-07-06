// SPDX-License-Identifier: Apache-2.0

package chisel3.internal

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, LinkedHashMap, LinkedHashSet}
import scala.util.control.NoStackTrace
import scala.util.matching.Regex
import _root_.logger.Logger
import java.io.File
import java.util.regex.PatternSyntaxException
import scala.io.Source

import chisel3.experimental.{NoSourceInfo, SourceInfo, SourceLine, UnlocatableSourceInfo}

object ExceptionHelpers {

  /** Root packages that are not typically relevant to Chisel user code. */
  final val packageTrimlist: Set[String] = Set("chisel3", "scala", "java", "jdk", "sun", "sbt")

  /** The object name of Chisel's internal `Builder`. */
  final val builderName: String = chisel3.internal.Builder.getClass.getName

  /** Return a stack trace element that looks like `... (someMessage)`.
    * @param message an optional message to include
    */
  def ellipsis(message: Option[String] = None): StackTraceElement =
    new StackTraceElement("..", " ", message.getOrElse(""), -1)

  /** Utility methods that can be added to exceptions.
    */
  implicit class ThrowableHelpers(throwable: Throwable) {

    /** For an exception, mutably trim a stack trace to user code only.
      *
      * This does the following actions to the stack trace:
      *
      *   1. From the top, remove elements while the (root) package matches the packageTrimlist
      *   2. Optionally, from the bottom, remove elements until the class matches an anchor
      *   3. From the anchor (or the bottom), remove elements while the (root) package matches the packageTrimlist
      *
      * @param packageTrimlist packages that should be removed from the stack trace
      * @param anchor an optional class name at which user execution might begin, e.g., a main object
      * @return nothing as this mutates the exception directly
      */
    def trimStackTraceToUserCode(
      packageTrimlist: Set[String] = packageTrimlist,
      anchor:          Option[String] = Some(builderName)
    ): Unit = {
      def inTrimlist(ste: StackTraceElement) = {
        val packageName = ste.getClassName().takeWhile(_ != '.')
        packageTrimlist.contains(packageName)
      }

      // Step 1: Remove elements from the top in the package trimlist
      val trimStackTrace =
        ((a: Array[StackTraceElement]) => a.dropWhile(inTrimlist))
          // Step 2: Optionally remove elements from the bottom until the anchor
          .andThen(_.reverse)
          .andThen(a =>
            anchor match {
              case Some(b) => a.dropWhile(ste => !ste.getClassName.startsWith(b))
              case None    => a
            }
          )
          // Step 3: Remove elements from the bottom in the package trimlist
          .andThen(_.dropWhile(inTrimlist))
          // Step 4: Reverse back to the original order
          .andThen(_.reverse.toArray)
          // Step 5: Add ellipsis stack trace elements and "--full-stacktrace" info
          .andThen(a =>
            ellipsis() +:
              a :+
              ellipsis() :+
              ellipsis(
                Some("Stack trace trimmed to user code only. Rerun with --full-stacktrace to see the full stack trace")
              )
          )
          // Step 5: Mutate the stack trace in this exception
          .andThen(throwable.setStackTrace(_))

      val stackTrace = throwable.getStackTrace
      if (stackTrace.nonEmpty) {
        trimStackTrace(stackTrace)
      }
    }

  }
}

/** Filter for warnings that can suppress, keep as warning, or elevate to error
  *
  * May filter on source file or message, a None filter means "match everything"
  */
private[chisel3] case class WarningFilter(src: Option[Regex], msg: Option[Regex], action: WarningFilter.Action) {
  def applies(source: String, message: String): Boolean = {
    // Note that None.forall is true
    src.forall(_.matches(source)) && msg.forall(_.matches(message))
  }
}
private[chisel3] object WarningFilter {
  sealed trait Action
  case object Suppress extends Action
  case object Warn extends Action
  case object Error extends Action

  // Some helpers for error reporting
  private def actionOneOf = "must be one of ':e, :w, or :s'."
  private def categoryOneOf = "must be one of 'any', 'src', or 'msg'."

  // Default additions to the regular expressions
  private def msgRegexDefault(base: Regex): Regex = new Regex(s".*$base.*")
  // TODO find a better way to deal with line and column
  private def srcRegexDefault(base: Regex): Regex = new Regex(s"(.*/)?$base(:\\d*){0,2}")

  /** Parse a String into a [[WarningFilter]]
    *
    * @param value String to parse
    * @return Left on failure with index of invalid character and a message or Right of successfully built Warning Filters
    */
  def parse(value: String): Either[(Int, String), WarningFilter] = {
    val actionIdx = value.lastIndexOf(':')
    if (actionIdx < 0) {
      return Left(value.size - 1 -> s"Filter '$value' is missing an action, $actionOneOf")
    }
    val (filterStr, actionStr) = value.splitAt(actionIdx)
    val action: Action = actionStr match {
      case ":e" => Error
      case ":w" => Warn
      case ":s" => Suppress
      case other =>
        return Left(actionIdx -> s"Invalid action '$other', $actionOneOf")
    }
    val filterParts: List[String] = filterStr.split("&").toList
    // Add index for adding a carat in error reporting
    val partsWithIndex: List[(Int, String)] =
      filterParts
        .mapAccumulate(0) { case (idx, s) => (idx + 1 + s.length, (idx, s)) } // + 1 for removed '&'
        ._2
    // Check legality of the parts
    val parts: Map[String, Option[Regex]] =
      partsWithIndex
        .mapAccumulate(Set.empty[String]) {
          case (seen, (idx, str)) =>
            val catIdx = str.indexOf('=')
            val (category, remainder) = if (catIdx < 0) (str, "") else str.splitAt(catIdx + 1) // +1 to include = in cat
            val regex = category match {
              case "any" =>
                // Any must be unique
                if (seen.nonEmpty) return Left(idx -> "'any' cannot be combined with other filters.")
                if (catIdx != -1) return Left(idx -> "'any' cannot have modifiers.")
                None
              // Note that split puts '=' with the category instead of regex
              case cat @ ("src=" | "msg=") =>
                if (seen("any")) return Left(idx -> "'any' cannot be combined with other filters.")
                if (seen(cat)) return Left(idx -> s"Cannot have duplicates of the same category.")
                try {
                  // Parse user input first so errors point to correct places
                  val baseRegex = new Regex(remainder)
                  // Add defaults to make API more friendly
                  Some(if (cat == "src=") srcRegexDefault(baseRegex) else msgRegexDefault(baseRegex))
                } catch {
                  case e: PatternSyntaxException =>
                    // Calculate offset to error, include category
                    val jdx = idx + cat.length + e.getIndex
                    return Left(jdx -> s"Invalid regular expression: ${e.getDescription}")
                }
              case other =>
                val cleanCat = if (other.last == '=') other.init else other // Drop trailing =
                return Left(idx -> s"Invalid category '$cleanCat', $categoryOneOf")
            }
            (seen + category, category -> regex)
        }
        ._2 // Discard seen
        .toMap
    if (parts.contains("any")) {
      Right(WarningFilter(None, None, action))
    } else {
      val src = parts.get("src=").flatten
      val msg = parts.get("msg=").flatten
      Right(WarningFilter(src, msg, action))
    }
  }
}

private[chisel3] class Errors(message: String) extends chisel3.ChiselException(message) with NoStackTrace

private[chisel3] object throwException {
  def apply(s: String, t: Throwable = null): Nothing =
    throw new chisel3.ChiselException(s, t)
}

private[chisel3] sealed trait UseColor
private[chisel3] object UseColor {
  case object Enabled extends UseColor
  case object Disabled extends UseColor
  case class Error(value: String) extends UseColor

  val envVar = "CHISEL_USE_COLOR"

  val value: UseColor = sys.env.get(envVar) match {
    case None =>
      val detect = System.console() != null && sys.env.get("TERM").exists(_ != "dumb")
      if (detect) Enabled else Disabled
    case Some("true")  => Enabled
    case Some("false") => Disabled
    case Some(other)   => Error(other)
  }

  def useColor: Boolean = value match {
    case Enabled  => true
    case Disabled => false
    case Error(_) => false
  }
}

/** Records and reports runtime errors and warnings. */
private[chisel3] object ErrorLog {
  def withColor(color: String, message: String): String =
    if (UseColor.useColor) color + message + Console.RESET else message
  val depTag = "[" + withColor(Console.BLUE, "deprecated") + "]"
  val warnTag = "[" + withColor(Console.YELLOW, "warn") + "]"
  val errTag = "[" + withColor(Console.RED, "error") + "]"
}

private[chisel3] class ErrorLog(
  warningFilters:    Seq[WarningFilter],
  sourceRoots:       Seq[File],
  throwOnFirstError: Boolean) {
  import ErrorLog.withColor

  private def getErrorLineInFile(sl: SourceLine): List[String] = {
    def tryFileInSourceRoot(sourceRoot: File): Option[List[String]] = {
      try {
        val file = new File(sourceRoot, sl.filename)
        val lines = Source.fromFile(file).getLines()
        var i = 0
        while (i < (sl.line - 1) && lines.hasNext) {
          lines.next()
          i += 1
        }
        val line = lines.next()
        val caretLine = (" " * (sl.col - 1)) + "^"
        Some(line :: caretLine :: Nil)
      } catch {
        case scala.util.control.NonFatal(_) => None
      }
    }
    val sourceRootsWithDefault = if (sourceRoots.nonEmpty) sourceRoots else Seq(new File("."))
    // View allows us to search the directories one at a time and early out
    sourceRootsWithDefault.view.map(tryFileInSourceRoot(_)).collectFirst { case Some(value) => value }.getOrElse(Nil)
  }

  /** Returns an appropriate location string for the provided source info.
    * If the source info is of `NoSourceInfo` type, the source location is looked up via stack trace.
    * If the source info is `None`, an empty string is returned.
    */
  private def errorLocationString(si: Option[SourceInfo]): String = {
    si match {
      case Some(sl: SourceLine) => s"${sl.filename}:${sl.line}:${sl.col}"
      case Some(_: NoSourceInfo) => {
        getUserLineNumber match {
          case Some(elt: StackTraceElement) => s"${elt.getFileName}:${elt.getLineNumber}"
          case None => "(unknown)"
        }
      }
      case None => ""
    }
  }

  // TODO refactor this to just hold more information in the ErrorEntry and do extra processing at report time
  private def errorEntry(msg: String, si: Option[SourceInfo], isFatal: Boolean): Option[ErrorEntry] = {
    val location = errorLocationString(si)
    val sourceLineAndCaret = si.collect { case sl: SourceLine => getErrorLineInFile(sl) }.getOrElse(Nil)
    val fullMessage = if (location.isEmpty) msg else s"$location: $msg"
    val errorLines = fullMessage :: sourceLineAndCaret

    val action =
      if (isFatal) WarningFilter.Error
      else {
        warningFilters.collectFirst { case wf if wf.applies(location, msg) => wf.action }
          .getOrElse(if (isFatal) WarningFilter.Error else WarningFilter.Warn)
      }
    action match {
      case WarningFilter.Error =>
        val entry = ErrorEntry(errorLines, true)
        if (throwOnFirstError) {
          throwException(entry.serialize(includeTag = false))
        }
        Some(entry)
      case WarningFilter.Warn     => Some(ErrorEntry(errorLines, false))
      case WarningFilter.Suppress => None
    }

  }

  /** Log an error message */
  def error(m: String, si: SourceInfo): Unit = {
    errors ++= errorEntry(m, Some(si), true)
  }

  private def warn(m: String, si: Option[SourceInfo]): Option[ErrorEntry] = errorEntry(m, si, false)

  /** Log a warning message */
  def warning(m: String, si: SourceInfo): Unit = {
    errors ++= warn(m, Some(si))
  }

  /** Log a warning message without a source locator. This is used when the
    * locator wouldn't be helpful (e.g., due to lazy values).
    */
  def warningNoLoc(m: String): Unit =
    errors ++= warn(m, None)

  /** Log a deprecation warning message */
  def deprecated(m: String, location: Option[String]): Unit = {
    val sourceLoc = location match {
      case Some(loc) => loc
      case None      => errorLocationString(Some(UnlocatableSourceInfo))
    }

    val thisEntry = (m, sourceLoc)
    deprecations += ((thisEntry, deprecations.getOrElse(thisEntry, 0) + 1))
  }

  /** Throw an exception if any errors have yet occurred. */
  def checkpoint(logger: Logger): Unit = {
    UseColor.value match {
      case UseColor.Error(value) =>
        logger.error(
          s"[error] Invalid value for environment variable '${UseColor.envVar}', must be 'true', 'false', or not set!"
        )
      case _ =>
    }

    deprecations.foreach {
      case ((message, sourceLoc), count) =>
        logger.warn(s"${ErrorLog.depTag} $sourceLoc ($count calls): $message")
    }
    errors.foreach(e => logger.error(e.serialize(includeTag = true)))

    if (!deprecations.isEmpty) {
      logger.warn(
        s"${ErrorLog.warnTag} " + withColor(
          Console.YELLOW,
          s"There were ${deprecations.size} deprecated function(s) used." +
            " These may stop compiling in a future release - you are encouraged to fix these issues."
        )
      )
      logger.warn(
        s"${ErrorLog.warnTag} Line numbers for deprecations reported by Chisel may be inaccurate; enable scalac compiler deprecation warnings via either of the following methods:"
      )
      logger.warn(s"${ErrorLog.warnTag}   In the sbt interactive console, enter:")
      logger.warn(s"""${ErrorLog.warnTag}     set scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")""")
      logger.warn(s"${ErrorLog.warnTag}   or, in your build.sbt, add the line:")
      logger.warn(s"""${ErrorLog.warnTag}     scalacOptions := Seq("-unchecked", "-deprecation")""")
    }

    val allErrors = errors.filter(_.isFatal)
    val allWarnings = errors.filter(!_.isFatal)

    if (!allWarnings.isEmpty && !allErrors.isEmpty) {
      logger.warn(
        s"${ErrorLog.errTag} There were " + withColor(Console.RED, s"${allErrors.size} error(s)") + " and " + withColor(
          Console.YELLOW,
          s"${allWarnings.size} warning(s)"
        ) + " during hardware elaboration."
      )
    } else if (!allWarnings.isEmpty) {
      logger.warn(
        s"${ErrorLog.warnTag} There were " + withColor(
          Console.YELLOW,
          s"${allWarnings.size} warning(s)"
        ) + " during hardware elaboration."
      )
    } else if (!allErrors.isEmpty) {
      logger.warn(
        s"${ErrorLog.errTag} There were " + withColor(
          Console.RED,
          s"${allErrors.size} error(s)"
        ) + " during hardware elaboration."
      )
    }

    if (!allErrors.isEmpty) {
      throw new Errors(
        "Fatal errors during hardware elaboration. Look above for error list. " +
          "Rerun with --throw-on-first-error if you wish to see a stack trace."
      )
    } else {
      // No fatal errors, clear accumulated warnings since they've been reported
      errors.clear()
    }
  }

  /** Returns the best guess at the first stack frame that belongs to user code.
    */
  private def getUserLineNumber = {
    def isChiselClassname(className: String): Boolean = {
      // List of classpath prefixes that are Chisel internals and should be ignored when looking for user code
      // utils are not part of internals and errors there can be reported
      val chiselPrefixes = Set(
        "java.",
        "scala.",
        "chisel3.",
        "chisel3.internal.",
        "chisel3.experimental.",
        "chisel3.package$" // for some compatibility / deprecated types
      )
      !chiselPrefixes.filter(className.startsWith(_)).isEmpty
    }

    Thread
      .currentThread()
      .getStackTrace
      .toList
      .dropWhile(
        // Get rid of everything in Chisel core
        ste => isChiselClassname(ste.getClassName)
      )
      .headOption
  }

  private val errors = LinkedHashSet[ErrorEntry]()
  private val deprecations = LinkedHashMap[(String, String), Int]()

  private val startTime = System.currentTimeMillis
  private def elapsedTime: Long = System.currentTimeMillis - startTime
}

private case class ErrorEntry(lines: Seq[String], isFatal: Boolean) {
  def tag = if (isFatal) ErrorLog.errTag else ErrorLog.warnTag

  def serialize(includeTag: Boolean): String = {
    val linesx = if (includeTag) lines.map(s"$tag " + _) else lines
    linesx.mkString("\n")
  }
}
