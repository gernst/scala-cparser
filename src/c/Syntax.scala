package c

case class Field(typ: Type, name: String) extends beaver.Symbol
case class Param(typ: Type, name: String) extends beaver.Symbol

sealed trait Global extends beaver.Symbol

case class TypeDef(typ: Type, name: String) extends Global
case class StructDef(name: String, fields: Array[Field]) extends Global
case class UnionDef(name: String, fields: Array[Field]) extends Global
case class EnumDef(name: String, cases: Array[String]) extends Global
case class VarDef(typ: Type, name: String, init: Option[Expr]) extends Global with Stmt {
  def this(typ: Type, name: String) = this(typ, name, None)
  def this(typ: Type, name: String, init: Expr) = this(typ, name, Some(init))
}
case class FunDef(ret: Type, name: String, params: Array[Param], body: Option[Block]) extends Global

sealed trait Type extends beaver.Symbol
sealed trait BaseType extends Type { def self = this }
sealed trait TypeName extends Type { def name: String }

case object Void extends BaseType

case object SChar extends BaseType
case object SShort extends BaseType
case object SInt extends BaseType
case object SLong extends BaseType

case object UChar extends BaseType
case object UShort extends BaseType
case object UInt extends BaseType
case object ULong extends BaseType

case class TypedefName(name: String) extends TypeName
case class StructName(name: String) extends TypeName
case class UnionName(name: String) extends TypeName
case class EnumName(name: String) extends TypeName

case class PtrType(typ: Type) extends Type

case class StructType(fields: Array[Field]) extends Type
case class UnionType(cases: Array[Field]) extends Type
case class EnumType(consts: Array[String]) extends Type

sealed trait Expr extends beaver.Symbol

case class Id(name: String) extends Expr
case class Lit(v: Any) extends Expr

case class PreOp(op: String, arg: Expr) extends Expr // op arg
case class PostOp(op: String, arg: Expr) extends Expr // arg op
case class BinOp(op: String, arg1: Expr, arg2: Expr) extends Expr // arg1 op arg2
case class AssignOp(op: String, arg1: Expr, arg2: Expr) extends Expr // arg1 op arg2
case class Question(test: Expr, left: Expr, right: Expr) extends Expr // test ? left : right

case class SizeOfType(typ: Type) extends Expr
case class SizeOfExpr(expr: Expr) extends Expr
case class Cast(typ: Type, expr: Expr) extends Expr

case class Lookup(expr: Expr, field: String) extends Expr // expr -> field
case class Index(expr: Expr, index: Expr) extends Expr // expr[index]

case class FunCall(name: String, args: Array[Expr]) extends Expr // no function pointers

case class Init(values: Array[(Option[String], Expr)]) extends Expr // { .field = value } or { value }

case class Block(stmts: Array[Stmt]) extends beaver.Symbol

sealed trait Stmt extends beaver.Symbol
case class Atomic(expr: Expr) extends Stmt
case object Break extends Stmt { def self = this }
case object Continue extends Stmt { def self = this }

case class Return(expr: Option[Expr]) extends Stmt {
  def this(expr: Expr) = this(Some(expr))
}

object Return extends (Option[Expr] => Return) {
  val self = Return(None)
}

case class If(test: Expr, left: Block, right: Option[Block]) extends Stmt {
  def this(test: Expr, left: Block) = this(test, left, None)
  def this(test: Expr, left: Block, right: Block) = this(test, left, Some(right))
}

case class While(test: Expr, body: Block) extends Stmt
case class DoWhile(body: Block, test: Expr) extends Stmt
case class For(init: Expr, test: Expr, inc: Expr, body: Block) extends Stmt

object Syntax {
  def modifies(expr: Expr): Set[Id] = expr match {
    case _: Id => Set()
    case _: Lit => Set()
    case PreOp("&", id: Id) => Set(id)
    case PreOp("++", id: Id) => Set(id)
    case PreOp("--", id: Id) => Set(id)
    case PostOp("++", id: Id) => Set(id)
    case PostOp("--", id: Id) => Set(id)
    case PreOp(op, arg) => modifies(arg)
    case PostOp(op, arg) => modifies(arg)
    case BinOp(op, arg1, arg2) => modifies(arg1) ++ modifies(arg2)
    case AssignOp(op, id: Id, arg) => Set(id) ++ modifies(arg)
    case AssignOp(op, arg1, arg2) => modifies(arg1) ++ modifies(arg2)
    case Question(test, left, right) => modifies(test) ++ modifies(left) ++ modifies(right)
    case Cast(typ, expr) => modifies(expr)
    case SizeOfExpr(expr) => Set() // compile time
    case SizeOfType(typ) => Set()
    case Lookup(expr, field) => modifies(expr)
    case Index(expr, index) => modifies(expr) ++ modifies(index)
    case FunCall(name, args) => Set() ++ (args flatMap modifies)
    case Init(values) => Set() ++ (values flatMap { case (_, expr) => modifies(expr) })
  }

  def hasEffects(expr: Expr): Boolean = expr match {
    case _: Id => false
    case _: Lit => false
    case PreOp("&", arg) => true // XXX: overapproximation
    case PreOp("++", arg) => true
    case PreOp("--", arg) => true
    case PostOp("++", arg) => true
    case PostOp("--", arg) => true
    case PreOp(op, arg) => hasEffects(arg)
    case PostOp(op, arg) => hasEffects(arg)
    case BinOp(op, arg1, arg2) => hasEffects(arg1) || hasEffects(arg2)
    case AssignOp(op, arg1, arg2) => true
    case Question(test, left, right) => hasEffects(test) || hasEffects(left) || hasEffects(right)
    case Cast(typ, expr) => hasEffects(expr)
    case SizeOfExpr(expr) => false // compile time
    case SizeOfType(typ) => false
    case Lookup(expr, field) => hasEffects(expr)
    case Index(expr, index) => hasEffects(expr) || hasEffects(index)
    case FunCall(name, args) => true // XXX: approximation
    case Init(values) => (values exists { case (_, expr) => hasEffects(expr) })
  }

  def modifies(block: Block): Set[Id] = block match {
    case Block(stmts) => Set() ++ (stmts flatMap modifies)
  }

  def modifies(stmt: Stmt): Set[Id] = stmt match {
    case _: VarDef => Set()
    case Atomic(expr) => modifies(expr)
    case Return(None) => Set()
    case Return(Some(expr)) => modifies(expr)
    case If(test, left, None) => modifies(test) ++ modifies(left)
    case If(test, left, Some(right)) => modifies(test) ++ modifies(left) ++ modifies(right)
    case While(test, body) => modifies(test) ++ modifies(body)
    case For(init, test, inc, body) => modifies(init) ++ modifies(test) ++ modifies(inc) ++ modifies(body)
  }
}
