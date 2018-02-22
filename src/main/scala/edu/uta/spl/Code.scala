/****************************************************************************************************
 *
 * File: Code.scala
 * The IR code generator for SPL programs
 *
 ****************************************************************************************************/

package edu.uta.spl


abstract class CodeGenerator ( tc: TypeChecker )  {
  def typechecker: TypeChecker = tc
  def st: SymbolTable = tc.st
  def code ( e: Program ): IRstmt
  def allocate_variable ( name: String, var_type: Type, fname: String ): IRexp
}


class Code ( tc: TypeChecker ) extends CodeGenerator(tc) {

  var name_counter = 0

  /** generate a new name */
  def new_name ( name: String ): String = {
    name_counter += 1
    name + "_" + name_counter
  }

  /** IR code to be added at the end of program */
  var addedCode: List[IRstmt] = Nil

  def addCode ( code: IRstmt* ) {
    addedCode ++= code
  }

  /** allocate a new variable at the end of the current frame and return the access code */
  def allocate_variable ( name: String, var_type: Type, fname: String ): IRexp =
    st.lookup(fname) match {
      case Some(FuncDeclaration(rtp,params,label,level,min_offset))
        => // allocate variable at the next available offset in frame
           st.insert(name,VarDeclaration(var_type,level,min_offset))
           // the next available offset in frame is 4 bytes below
           st.replace(fname,FuncDeclaration(rtp,params,label,level,min_offset-4))
           // return the code that accesses the variable
           Mem(Binop("PLUS",Reg("fp"),IntValue(min_offset)))
      case _ => throw new Error("No current function: " + fname)
    }

  /** access a frame-allocated variable from the run-time stack */
  def access_variable ( name: String, level: Int ): IRexp =
    st.lookup(name) match {
      case Some(VarDeclaration(_,var_level,offset))
        => var res: IRexp = Reg("fp")
           // non-local variable: follow the static link (level-var_level) times
           for ( i <- var_level+1 to level )
               res = Mem(Binop("PLUS",res,IntValue(-8)))
           Mem(Binop("PLUS",res,IntValue(offset)))
      case _ => throw new Error("Undefined variable: " + name)
    }

  /** return the IR code from the Expr e (level is the current function nesting level,
   *  fname is the name of the current function/procedure) */
  def code ( e: Expr, level: Int, fname: String ): IRexp =
    e match {
      case BinOpExp(op,left,right)
        => val cl = code(left,level,fname)
           val cr = code(right,level,fname)
           val nop = op.toUpperCase()
           Binop(nop,cl,cr)
      case ArrayGen(len,v)
        => val A = allocate_variable(new_name("A"),typechecker.typecheck(e),fname)
           val L = allocate_variable(new_name("L"),IntType(),fname)
           val V = allocate_variable(new_name("V"),typechecker.typecheck(v),fname)
           val I = allocate_variable(new_name("I"),IntType(),fname)
           val loop = new_name("loop")
           val exit = new_name("exit")
           ESeq(Seq(List(Move(L,code(len,level,fname)),   // store length in L
                         Move(A,Allocate(Binop("PLUS",L,IntValue(1)))),
                         Move(V,code(v,level,fname)),     // store value in V
                         Move(Mem(A),L),                  // store length in A[0]
                         Move(I,IntValue(0)),
                         Label(loop),                     // for-loop
                         CJump(Binop("GEQ",I,L),exit),
                         Move(Mem(Binop("PLUS",A,Binop("TIMES",Binop("PLUS",I,IntValue(1)),IntValue(4)))),V),  // A[i] = v
                         Move(I,Binop("PLUS",I,IntValue(1))),
                         Jump(loop),
                         Label(exit))),
                A)

      /* PUT YOUR CODE HERE */

      case _ => throw new Error("Wrong expression: "+e)
    }

  /** return the IR code from the Lvalue e (level is the current function nesting level,
   *  fname is the name of the current function/procedure) */
  def code ( e: Lvalue, level: Int, fname: String ): IRexp =
    e match {
     case RecordDeref(r,a)
        => val cr = code(r,level,fname)
           typechecker.expandType(typechecker.typecheck(r)) match {
              case RecordType(cl)
                => val i = cl.map(_.name).indexOf(a)
                   Mem(Binop("PLUS",cr,IntValue(i*4)))
              case _ => throw new Error("Unkown record: "+e)
           }

     /* PUT YOUR CODE HERE */

     case _ => throw new Error("Wrong statement: " + e)
    }

  /** return the IR code from the Statement e (level is the current function nesting level,
   *  fname is the name of the current function/procedure)
   *  and exit_label is the exit label       */
  def code ( e: Stmt, level: Int, fname: String, exit_label: String ): IRstmt =
    e match {
      case ForSt(v,a,b,c,s)
        => val loop = new_name("loop")
           val exit = new_name("exit")
           val cv = allocate_variable(v,IntType(),fname)
           val ca = code(a,level,fname)
           val cb = code(b,level,fname)
           val cc = code(c,level,fname)
           val cs = code(s,level,fname,exit)
           Seq(List(Move(cv,ca),  // needs cv, not Mem(cv)
                    Label(loop),
                    CJump(Binop("GT",cv,cb),exit),
                    cs,
                    Move(cv,Binop("PLUS",cv,cc)),  // needs cv, not Mem(cv)
                    Jump(loop),
                    Label(exit)))

      /* PUT YOUR CODE HERE */

      case _ => throw new Error("Wrong statement: " + e)
   }

  /** return the IR code for the declaration block of function fname
   * (level is the current function nesting level) */
  def code ( e: Definition, fname: String, level: Int ): IRstmt =
    e match {
      case FuncDef(f,ps,ot,b)
        => val flabel = if (f == "main") f else new_name(f)
           /* initial available offset in frame f is -12 */
           st.insert(f,FuncDeclaration(ot,ps,flabel,level+1,-12))
           st.begin_scope()
           /* formal parameters have positive offsets */
           ps.zipWithIndex.foreach{ case (Bind(v,tp),i)
                                      => st.insert(v,VarDeclaration(tp,level+1,(ps.length-i)*4)) }
           val body = code(b,level+1,f,"")
           st.end_scope()
           st.lookup(f) match {
             case Some(FuncDeclaration(_,_,_,_,offset))
               => addCode(Label(flabel),
                          /* prologue */
                          Move(Mem(Reg("sp")),Reg("fp")),
                          Move(Reg("fp"),Reg("sp")),
                          Move(Mem(Binop("PLUS",Reg("fp"),IntValue(-4))),Reg("ra")),
                          Move(Mem(Binop("PLUS",Reg("fp"),IntValue(-8))),Reg("v0")),
                          Move(Reg("sp"),Binop("PLUS",Reg("sp"),IntValue(offset))),
                          body,
                          /* epilogue */
                          Move(Reg("ra"),Mem(Binop("PLUS",Reg("fp"),IntValue(-4)))),
                          Move(Reg("sp"),Reg("fp")),
                          Move(Reg("fp"),Mem(Reg("fp"))),
                          Return())
                  Seq(List())
             case _ => throw new Error("Unkown function: "+f)
           }

      /* PUT YOUR CODE HERE */

      case _ => throw new Error("Wrong statement: " + e)
    }

    def code ( e: Program ): IRstmt =
      e match {
        case Program(b@BlockSt(_,_))
          => st.begin_scope()
             val res = code(FuncDef("main",List(),NoType(),b),"",0)
             st.end_scope()
             Seq(res::addedCode)
        case _ => throw new Error("Wrong program "+e);
      }
}
