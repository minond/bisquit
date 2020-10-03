package bisquit
package errors

import input._
import nodes._
import typechecker._

import scala.reflect.ClassTag


trait BisquitError

trait ParsingError extends Positioned with BisquitError
case class UnexpectedToken[Expected: ClassTag](got: Token) extends ParsingError
case class UnexpectedExpression[Expected: ClassTag](got: Expression) extends ParsingError
case class UnexpectedEOF() extends ParsingError
case class StringNotClosed() extends ParsingError
case class InvalidInteger(lexeme: String) extends ParsingError
case class InvalidCharacter(c: Char) extends ParsingError

case class FileNotFound(name: String, loadPaths: List[String]) extends BisquitError

sealed trait TypingError extends BisquitError
case class LookupError(id: Id) extends TypingError
case class UnificationError(ty1: Type, ty2: Type) extends TypingError
case class ExpectedRecordInstead(got: Type) extends TypingError
case class ExpectedCallableInstead(got: Expression) extends TypingError
case class TooManyArguments(fnTy: LambdaType, args: List[Expression]) extends TypingError
case class RecordLookupError(id: Id, record: Type) extends TypingError

sealed trait RuntimeError extends BisquitError
case class RuntimeLookupError(id: Id) extends RuntimeError
case class ArgumentTypeError(arg: IR) extends RuntimeError
case class CannotGetCarOfEmptyList(list: IR) extends RuntimeError
case class ConditionError(cond: IR) extends RuntimeError
case class RuntimeRecordLookupError(id: Id, record: Record) extends RuntimeError
case class RuntimeExpectedRecordInstead(got: Value) extends RuntimeError
case class ModuleValueNotExposed(id: Id, module: Module) extends RuntimeError
case class DuplicateExposeName(id: Id) extends RuntimeError
case class IncorrectModuleName(found: Id, expected: Id) extends RuntimeError
