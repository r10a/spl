package edu.uta.spl

abstract class TypeChecker {
  var trace_typecheck = false

  /** symbol table to store SPL declarations */
  var st = new SymbolTable

  def expandType ( tp: Type ): Type
  def typecheck ( e: Expr ): Type
  def typecheck ( e: Lvalue ): Type
  def typecheck ( e: Stmt, expected_type: Type )
  def typecheck ( e: Definition )
  def typecheck ( e: Program )
}


class TypeCheck extends TypeChecker {

  /** typechecking error */
  def error ( msg: String ): Type = {
    System.err.println("*** Typechecking Error: "+msg)
    System.err.println("*** Symbol Table: "+st)
    System.exit(1)
    null
  }

  /** if tp is a named type, expand it */
  def expandType ( tp: Type ): Type =
    tp match {
      case NamedType(nm)
        => st.lookup(nm) match {
          case Some(TypeDeclaration(t))
              => expandType(t)
          case _ => error("Undeclared type: "+tp)
        }
      case _ => tp
  }

  /** returns true if the types tp1 and tp2 are equal under structural equivalence */
  def typeEquivalence ( tp1: Type, tp2: Type ): Boolean =
    if (tp1 == tp2 || tp1.isInstanceOf[AnyType] || tp2.isInstanceOf[AnyType])
      true
    else expandType(tp1) match {
      case ArrayType(t1)
        => expandType(tp2) match {
              case ArrayType(t2)
                => typeEquivalence(t1,t2)
              case _ => false
           }
      case RecordType(fs1)
        => expandType(tp2) match {
              case RecordType(fs2)
                => fs1.length == fs2.length &&
                   (fs1 zip fs2).map{ case (Bind(v1,t1),Bind(v2,t2))
                                        => v1==v2 && typeEquivalence(t1,t2) }
                                .reduce(_&&_)
              case _ => false
           }
      case TupleType(ts1)
        => expandType(tp2) match {
              case TupleType(ts2)
                => ts1.length == ts2.length &&
                   (ts1 zip ts2).map{ case (t1,t2) => typeEquivalence(t1,t2) }
                                .reduce(_&&_)
              case _ => false
           }
      case _
        => tp2 match {
             case NamedType(n) => typeEquivalence(tp1,expandType(tp2))
             case _ => false
           }
    }

  /* tracing level */
  var level: Int = -1

  /** trace typechecking */
  def trace[T] ( e: Any, result: => T ): T = {
    if (trace_typecheck) {
       level += 1
       println(" "*(3*level)+"** "+e)
    }
    val res = result
    if (trace_typecheck) {
       print(" "*(3*level))
       if (e.isInstanceOf[Stmt] || e.isInstanceOf[Definition])
          println("->")
       else println("-> "+res)
       level -= 1
    }
    res
  }

  /** typecheck an expression AST */
  def typecheck ( e: Expr ): Type =
    trace(e,e match {
      case BinOpExp(op,l,r)
        => val ltp = typecheck(l)
           val rtp = typecheck(r)
           if (!typeEquivalence(ltp,rtp))
              error("Incompatible types in binary operation: "+e)
           else if (op.equals("and") || op.equals("or"))
                   if (typeEquivalence(ltp,BooleanType()))
                      ltp
                   else error("AND/OR operation can only be applied to booleans: "+e)
           else if (op.equals("eq") || op.equals("neq"))
                   BooleanType()
           else if (!typeEquivalence(ltp,IntType()) && !typeEquivalence(ltp,FloatType()))
                   error("Binary arithmetic operations can only be applied to integer or real numbers: "+e)
           else if (op.equals("gt") || op.equals("lt") || op.equals("geq") || op.equals("leq"))
                   BooleanType()
           else ltp

      /* PUT YOUR CODE HERE */

      case _ => throw new Error("Wrong expression: "+e)
    } )

  /** typecheck an Lvalue AST */
  def typecheck ( e: Lvalue ): Type =
    trace(e,e match {
      case Var(name)
        => st.lookup(name) match {
              case Some(VarDeclaration(t,_,_)) => t
              case Some(_) => error(name+" is not a variable")
              case None => error("Undefined variable: "+name)
        }

      /* PUT YOUR CODE HERE */

      case _ => throw new Error("Wrong lvalue: "+e)
    } )

  /** typecheck a statement AST using the expected type of the return value from the current function */
  def typecheck ( e: Stmt, expected_type: Type ) {
    trace(e,e match {
      case AssignSt(d,s)
        => if (!typeEquivalence(typecheck(d),typecheck(s)))
              error("Incompatible types in assignment: "+e)

      /* PUT YOUR CODE HERE */

      case _ => throw new Error("Wrong statement: "+e)
    } )
  }

  /** typecheck a definition */
  def typecheck ( e: Definition ) {
    trace(e,e match {
      case FuncDef(f,ps,ot,b)
        => st.insert(f,FuncDeclaration(ot,ps,"",0,0))
           st.begin_scope()
           ps.foreach{ case Bind(v,tp) => st.insert(v,VarDeclaration(tp,0,0)) }
           typecheck(b,ot)
           st.end_scope()

      /* PUT YOUR CODE HERE */

      case _ => throw new Error("Wrong statement: "+e)
    } )
  }

  /** typecheck the main program */
  def typecheck ( e: Program ) {
    typecheck(e.body,NoType())
  }
}
