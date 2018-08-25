package edu.uta.spl

abstract class TypeChecker {
  var trace_typecheck = false

  /** symbol table to store SPL declarations */
  var st = new SymbolTable

  def expandType(tp: Type): Type
  def typecheck(e: Expr): Type
  def typecheck(e: Lvalue): Type
  def typecheck(e: Stmt, expected_type: Type)
  def typecheck(e: Definition)
  def typecheck(e: Program)
}

class TypeCheck extends TypeChecker {

  /** typechecking error */
  def error(msg: String): Type = {
    System.err.println("*** Typechecking Error: " + msg)
    System.err.println("*** Symbol Table: " + st)
    System.exit(1)
    null
  }

  /** if tp is a named type, expand it */
  def expandType(tp: Type): Type =
    tp match {
      case NamedType(nm) => st.lookup(nm) match {
        case Some(TypeDeclaration(t)) => expandType(t)
        case _                        => error("Undeclared type: " + tp)
      }
      case _ => tp
    }

  /** returns true if the types tp1 and tp2 are equal under structural equivalence */
  def typeEquivalence(tp1: Type, tp2: Type): Boolean =
    if (tp1 == tp2 || tp1.isInstanceOf[AnyType] || tp2.isInstanceOf[AnyType])
      true
    else expandType(tp1) match {
      case ArrayType(t1) => expandType(tp2) match {
        case ArrayType(t2) => typeEquivalence(t1, t2)
        case _             => false
      }
      case RecordType(fs1) => expandType(tp2) match {
        case RecordType(fs2) => fs1.length == fs2.length &&
          (fs1 zip fs2).map {
            case (Bind(v1, t1), Bind(v2, t2)) => v1 == v2 && typeEquivalence(t1, t2)
          }
          .reduce(_ && _)
        case _ => false
      }
      case TupleType(ts1) => expandType(tp2) match {
        case TupleType(ts2) => ts1.length == ts2.length &&
          (ts1 zip ts2).map { case (t1, t2) => typeEquivalence(t1, t2) }
          .reduce(_ && _)
        case _ => false
      }
      case _ => tp2 match {
        case NamedType(n) => typeEquivalence(tp1, expandType(tp2))
        case _            => false
      }
    }

  /* tracing level */
  var level: Int = -1

  /** trace typechecking */
  def trace[T](e: Any, result: => T): T = {
    if (trace_typecheck) {
      level += 1
      println(" " * (3 * level) + "** " + e)
    }
    val res = result
    if (trace_typecheck) {
      print(" " * (3 * level))
      if (e.isInstanceOf[Stmt] || e.isInstanceOf[Definition])
        println("->")
      else println("-> " + res)
      level -= 1
    }
    res
  }

  /** typecheck an expression AST */
  def typecheck(e: Expr): Type =
    trace(e, e match {
      case BinOpExp(op, l, r) =>
        val ltp = typecheck(l)
        val rtp = typecheck(r)
        if (!typeEquivalence(ltp, rtp))
          error("Incompatible types in binary operation: " + e)
        else if (op.equals("and") || op.equals("or"))
          if (typeEquivalence(ltp, BooleanType()))
            ltp
          else error("AND/OR operation can only be applied to booleans: " + e)
        else if (op.equals("eq") || op.equals("neq"))
          BooleanType()
        else if (!typeEquivalence(ltp, IntType()) && !typeEquivalence(ltp, FloatType()))
          error("Binary arithmetic operations can only be applied to integer or real numbers: " + e)
        else if (op.equals("gt") || op.equals("lt") || op.equals("geq") || op.equals("leq"))
          BooleanType()
        else ltp

      /* PUT YOUR CODE HERE */

      case UnOpExp(op, e) =>
        val etp = typecheck(e)
        if (typeEquivalence(etp, BooleanType()) && op.equals("not"))
          etp
        else if ((typeEquivalence(etp, IntType()) || typeEquivalence(etp, FloatType())) && op.equals("minus"))
          etp
        else
          error("Incompatible types in unary operation: " + e)

      case e: StringConst  => StringType()

      case e: IntConst     => IntType()

      case e: BooleanConst => BooleanType()

      case LvalExp(value)  => typecheck(value)

      case ArrayExp(elements) =>
        var typ: Type = null
        elements.foreach(e => {
          typ = typecheck(e)
        })
        ArrayType(typ)

      case ArrayGen(length, value) =>
        typecheck(length)
        ArrayType(typecheck(value))

      case CallExp(name, args) =>
        args.foreach(a => typecheck(a))
        st.lookup(name).get match {
          case a: FuncDeclaration => a.outtype
          case _                  => error("Invalid Call expression: " + name)
        }

      case RecordExp(components) => RecordType(components.map { case Bind(l, r) => Bind(l, typecheck(r)) })

      case NullExp()             => AnyType()

      case _                     => throw new Error("Wrong expression: " + e)
    })

  /** typecheck an Lvalue AST */
  def typecheck(e: Lvalue): Type =
    trace(e, e match {
      case Var(name) => st.lookup(name) match {
        case Some(VarDeclaration(vartype, _, _)) => vartype
        case Some(_)                             => error(name + " is not a variable")
        case None                                => error("Undefined variable: " + name)
      }

      /* PUT YOUR CODE HERE */
      case ArrayDeref(array, index) =>
        typecheck(index)
        typecheck(array) match {
          case ArrayType(element_type) => element_type
          case x                       => x
        }

      case RecordDeref(record, name) =>
        val typ = typecheck(record)
        var final_type: Type = null;
        typ match {
          case NamedType(typename) =>
            st.lookup(typename) match {
              case Some(TypeDeclaration(RecordType(components))) => components.foreach {
                case a: Bind[Type] => if (a.name == name) final_type = a.value
              }
              case Some(_) => error("Invalid Type: " + record)
              case None    => error("Type Not Found: " + typ)
            }
          case RecordType(components) => components.foreach {
            case a: Bind[Type] => if (a.name == name) final_type = a.value
          }
          case _ => error("Invalid Record: " + record + " : " + typ)
        }
        final_type

      case _ => throw new Error("Wrong lvalue: " + e)
    })

  /** typecheck a statement AST using the expected type of the return value from the current function */
  def typecheck(e: Stmt, expected_type: Type) {
    trace(e, e match {
      case AssignSt(d, s) => if (!typeEquivalence(typecheck(d), typecheck(s)))
        error("Incompatible types in assignment: " + e)

      /* PUT YOUR CODE HERE */
      case BlockSt(ds, ss) =>
        st.begin_scope()
        ds.foreach(d => typecheck(d))
        ss.foreach(s => typecheck(s, expected_type))
        st.end_scope()

      case PrintSt(es) => es.foreach(e => typecheck(e))

      case WhileSt(condition, body) =>
        typecheck(condition)
        typecheck(body, expected_type)

      case IfSt(condition, then, els) =>
        typecheck(condition)
        typecheck(then, expected_type)
        if (els != null) typecheck(els, expected_type)

      case CallSt(name, args) =>
        st.lookup(name)
        args.foreach(a => typecheck(a))

      case ReadSt(args) => args.foreach(a => typecheck(a))

      case ForSt(variable, initial, step, increment, body) =>
        st.begin_scope()
        st.insert(variable, VarDeclaration(typecheck(initial), 0, 0))
        typecheck(step)
        typecheck(increment)
        typecheck(body, expected_type)
        st.end_scope()

      case LoopSt(body) => typecheck(body, expected_type)

      case ReturnValueSt(exp) =>
        val t = typecheck(exp)
        if (expected_type == NoType())
          error("Cannot return a value from a procedure: " + e)
        else if (!typeEquivalence(t, expected_type))
          error("Invalid return type: " + e + " | Actual return Type : " + t + " | Expected : " + expected_type)
      /*else // Debugging
          println("Equal: " + t + " : " + expected_type)*/

      case ExitSt()   =>

      case ReturnSt() =>

      case _          => throw new Error("Wrong statement: " + e)
    })
  }

  /** typecheck a definition */
  def typecheck(e: Definition) {
    trace(e, e match {
      case FuncDef(f, ps, ot, b) =>
        st.insert(f, FuncDeclaration(ot, ps, "", 0, 0))
        st.begin_scope()
        ps.foreach { case Bind(v, tp) => st.insert(v, VarDeclaration(tp, 0, 0)) }
        typecheck(b, ot)
        st.end_scope()

      /* PUT YOUR CODE HERE */
      case VarDef(name, hasType, e) =>
        val t = typecheck(e)
        if (hasType != AnyType()) {
          if (typeEquivalence(hasType, t))
            st.insert(name, VarDeclaration(hasType, 0, 0))
          else
            throw new Error("Wrong statement: " + name)
        } else {
          st.insert(name, VarDeclaration(t, 0, 0))
        }

      case TypeDef(name, typ) =>
        st.insert(name, TypeDeclaration(typ))

      case _ => throw new Error("Wrong statement: " + e)
    })
  }

  /** typecheck the main program */
  def typecheck(e: Program) {
    typecheck(e.body, NoType())
  }
}