package edu.uta.spl

/** Abstract syntax tree for the main program */
case class Program ( body: BlockSt )

/** a name-value pair */
case class Bind[T] ( name: String, value: T )

/** Abstract syntax trees for definitions */
sealed abstract class Definition
/** a type definition */
case class TypeDef ( name: String, isType: Type ) extends Definition
/** a variable definition */
case class VarDef ( name: String, hasType: Type, value: Expr ) extends Definition
/** a function definition */
case class FuncDef ( name: String, formal_params: List[Bind[Type]], result_type: Type, body: BlockSt ) extends Definition

/** Abstract syntax trees for types */
sealed abstract class Type
case class IntType () extends Type
case class FloatType () extends Type
case class StringType () extends Type
case class BooleanType () extends Type
case class NamedType ( typename: String ) extends Type
case class ArrayType ( element_type: Type ) extends Type
case class RecordType ( components: List[Bind[Type]] ) extends Type
case class TupleType ( components: List[Type] ) extends Type
case class AnyType () extends Type
case class NoType () extends Type

/** Abstract syntax trees for lvalues */
sealed abstract class Lvalue
case class Var ( name: String ) extends Lvalue
case class ArrayDeref ( array: Expr, index: Expr ) extends Lvalue
case class RecordDeref ( record: Expr, attribute: String ) extends Lvalue
case class TupleDeref ( tuple: Expr, index: Int ) extends Lvalue

/** Abstract syntax trees for expressions */
sealed abstract class Expr
case class IntConst ( value: Int ) extends Expr
case class FloatConst ( value: Float ) extends Expr
case class StringConst ( value: String ) extends Expr
case class BooleanConst ( value: Boolean ) extends Expr
case class LvalExp ( value: Lvalue ) extends Expr
case class NullExp () extends Expr
case class BinOpExp ( op: String, left: Expr, right: Expr ) extends Expr
case class UnOpExp ( op: String, operand: Expr ) extends Expr
case class CallExp ( name: String, arguments: List[Expr] ) extends Expr
case class RecordExp ( components: List[Bind[Expr]] ) extends Expr
case class ArrayExp ( elements: List[Expr] ) extends Expr
case class ArrayGen ( length: Expr, value: Expr ) extends Expr
case class TupleExp ( elements: List[Expr] ) extends Expr

/** Abstract syntax trees for statements */
sealed abstract class Stmt
case class AssignSt ( destination: Lvalue, source: Expr ) extends Stmt
case class CallSt ( name: String, arguments: List[Expr] ) extends Stmt
case class ReadSt ( arguments: List[Lvalue] ) extends Stmt
case class PrintSt ( arguments: List[Expr] ) extends Stmt
case class IfSt ( condition: Expr, then_stmt: Stmt, else_stmt: Stmt ) extends Stmt
case class WhileSt ( condition: Expr, body: Stmt ) extends Stmt
case class LoopSt ( body: Stmt ) extends Stmt
case class ForSt ( variable: String, initial: Expr, step: Expr, increment: Expr, body: Stmt ) extends Stmt
case class ExitSt () extends Stmt
case class ReturnValueSt ( value: Expr ) extends Stmt
case class ReturnSt () extends Stmt
case class BlockSt ( decls: List[Definition], stmts: List[Stmt] ) extends Stmt
