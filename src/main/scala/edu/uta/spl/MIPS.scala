/****************************************************************************************************
 *
 * File: MIPS.scala
 * Generation of MIPS code from IR code
 *
 ****************************************************************************************************/

package edu.uta.spl

/** representation of a MIPS register */
case class Register(reg: String) {
  override def toString: String = reg
}

/** a pool of available registers */
class RegisterPool {

  val all_registers = List("$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8", "$t9",
    "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7")

  var available_registers: List[Register] = all_registers.map(Register)

  /** is register reg temporary? */
  def is_temporary(reg: Register): Boolean =
    reg match {
      case Register(n) => all_registers.contains(n)
    }

  /** return the next available temporary register */
  def get(): Register =
    available_registers match {
      case reg :: rs =>
        available_registers = rs
        reg
      case _ => throw new Error("*** Run out of registers")
    }

  /** recycle (put back into the register pool) the register reg (if is temporary) */
  def recycle(reg: Register) {
    if (available_registers.contains(reg))
      throw new Error("*** Register has already been recycled: " + reg)
    if (is_temporary(reg))
      available_registers = reg :: available_registers
  }

  /** return the list of all temporary registers currently in use */
  def used(): List[Register] = {
    for (reg <- all_registers if !available_registers.contains(Register(reg)))
      yield Register(reg)
  }
}

abstract class MipsGenerator {
  def clear()
  def emit(e: IRstmt)
  def initialCode()
}

class Mips extends MipsGenerator {

  /** emit a MIPS label */
  def mips_label(s: String) {
    SPL.out.println(s + ":")
  }

  /** emit MIPS code with no operands */
  def mips(op: String) {
    SPL.out.println("        " + op)
  }

  /** emit MIPS code with operands */
  def mips(op: String, args: String) {
    SPL.out.print("        " + op)
    for (i <- op.length to 10)
      SPL.out.print(" ")
    SPL.out.println(args)
  }

  /** a pool of temporary registers */
  var rpool = new RegisterPool

  /** clear the register pool */
  def clear {
    rpool = new RegisterPool
  }

  var name_counter = 0

  /** generate a new  label name */
  def new_label(): String = {
    name_counter += 1
    "L_" + name_counter
  }

  /** generate MIPS code from the IR expression e and return the register that will hold the result */
  def emit(e: IRexp): Register = {
    e match {
      case Mem(Binop("PLUS", Reg(r), IntValue(n))) =>
        val reg = rpool.get()
        mips("lw", reg + ", " + n + "($" + r + ")")
        reg
      case Binop("AND", x, y) =>
        val label = new_label()
        val left = emit(x)
        val reg = left
        mips("beq", left + ", 0, " + label)
        val right = emit(y)
        mips("move", left + ", " + right)
        mips_label(label)
        rpool.recycle(right)
        reg
      case Call(f, sl, args) =>
        val used_regs = rpool.used()
        val size = (used_regs.length + args.length) * 4
        /* allocate space for used temporary registers */
        if (size > 0)
          mips("subu", "$sp, $sp, " + size)
        /* push the used temporary registers */
        var i = size
        for (r <- used_regs) {
          mips("sw", r + ", " + i + "($sp)")
          i -= 4
        }
        /* push arguments */
        i = args.length * 4
        for (a <- args) {
          val reg = emit(a)
          mips("sw", reg + ", " + i + "($sp)")
          rpool.recycle(reg)
          i -= 4
        }
        /* set $v0 to be the static link */
        val sreg = emit(sl)
        mips("move", "$v0, " + sreg)
        rpool.recycle(sreg)
        mips("jal", f)
        i = size
        /* pop the used temporary registers */
        for (r <- used_regs) {
          mips("lw", r + ", " + i + "($sp)")
          i -= 4
        }
        /* deallocate stack from args and used temporary registers */
        if (size > 0)
          mips("addu", "$sp, $sp, " + size)
        val res = rpool.get()
        mips("move", res + ", $a0")
        /* You shouldn't just return $a0 */
        res

      /* PUT YOUR CODE HERE */
      case Reg(s) =>
        Register("$" + s)

      case IntValue(n) =>
        val tmp = rpool.get()
        mips("li", tmp + ", " + n)
        tmp

      case Allocate(IntValue(n)) =>
        val t1 = rpool.get()
        val t2 = rpool.get()
        mips("li", t1 + ", " + n)
        mips("li", t2 + ", 4")
        mips("mul", t1 + ", " + t1 + ", " + t2)
        mips("move", t2 + ", $gp")
        mips("addu", "$gp, $gp, " + t1)
        rpool.recycle(t1)
        t2

      case Allocate(u) =>
        val t1 = emit(u)
        val t2 = rpool.get()
        mips("li", t2 + ", 4")
        mips("mul", t1 + ", " + t1 + ", " + t2)
        mips("move", t2 + ", $gp")
        mips("addu", "$gp, $gp, " + t1)
        rpool.recycle(t1)
        t2

      case Binop(op, l, r) =>
        val li = emit(l)
        val ri = emit(r)
        val iop = op match {
          case "PLUS"  => "addu"
          case "TIMES" => "mul"
          case "MINUS" => "subu"
          case "LT"    => "slt"
          case "GT"    => "sgt"
          case "EQ"    => "seq"
          case "GEQ"   => "sge"
          case "LEQ"   => "sle"
          case "NEQ"   => "sne"
          case _       => throw new Error("*** Unknown opearation: " + op)
        }
        mips(iop, li + ", " + li + ", " + ri)
        rpool.recycle(ri)
        li

      case Mem(Binop("PLUS", u, IntValue(n))) =>
        val tmp = rpool.get()
        val src = emit(u)
        mips("lw", tmp + ", " + n + "(" + src + ")")
        rpool.recycle(src)
        tmp

      case Mem(u) =>
        val tmp = rpool.get()
        val src = emit(u)
        mips("lw", tmp + ", " + "(" + src + ")")
        rpool.recycle(src)
        tmp

      case Unop("NOT", u) =>
        val c = emit(u)
        mips("seq", c + ", " + c + ", 0")
        c

      case Unop("MINUS", u) =>
        val c = emit(u)
        mips("neg", c + ", " + c)
        c

      case _ => throw new Error("*** Unknown IR: " + e)
    }
  }

  /** generate MIPS code from the IR statement e */
  def emit(e: IRstmt) {
    e match {

      case Move(Mem(Binop("PLUS", Reg(r), IntValue(n))), u) =>
        val src = emit(u)
        mips("sw", src + ", " + n + "($" + r + ")")
        rpool.recycle(src)

      /* PUT YOUR CODE HERE */
      case Label(l) =>
        mips_label(l)

      case Move(Mem(Binop("PLUS", u, IntValue(index))), IntValue(n)) =>
        val dest = emit(u)
        val tmp1 = rpool.get()
        mips("li", tmp1 + ", " + n)
        mips("sw", tmp1 + ", " + index + "(" + dest + ")")
        rpool.recycle(tmp1)
        rpool.recycle(dest)

      case Move(Mem(d), s) =>
        val dest = emit(d)
        val src = emit(s)
        mips("sw", src + ", " + "(" + dest + ")")
        rpool.recycle(src)
        rpool.recycle(dest)

      case Move(Reg(l), Mem(Reg(s))) =>
        mips("lw", emit(Reg(l)) + ", (" + emit(Reg(s)) + ")")

      case Move(Reg(d), Reg(s)) =>
        val dest = emit(Reg(d))
        val src = emit(Reg(s))
        mips("move", dest + ", " + src)
        rpool.recycle(src)
        rpool.recycle(dest)

      case Move(Reg(sp), u) =>
        val dest = emit(Reg(sp))
        val src = emit(u)
        mips("move", dest + ", " + src)
        rpool.recycle(src)
        rpool.recycle(dest)

      case CJump(u, exit_label) =>
        val c = emit(u)
        mips("beq", c + ", 1, " + exit_label)
        rpool.recycle(c)

      case CallP(name, static_link, arguments) =>
        val r = emit(Call(name, static_link, arguments))
        rpool.recycle(r)

      case SystemCall(typ, value) =>
        typ match {
          case "WRITE_STRING" =>
            value match {
              case StringValue("\\n") =>
                mips("li", "$v0, 4")
                mips("la", "$a0, ENDL_")
                mips("syscall")
                
              case StringValue(s) =>
                val label = new_label()
                val tmp = rpool.get()
                mips(".data")
                mips(".align", 2.toString())
                mips_label(label)
                mips(".asciiz", "\"" + s + "\"")
                mips(".text")
                mips("la", tmp + ", " + label)
                mips("move", "$a0, " + tmp)
                mips("li", "$v0, 4")
                mips("syscall")
                rpool.recycle(tmp)

              case _ => throw new Error("*** Unknown Write" + typ + value + e)
            }

          case "WRITE_INT" =>
            value match {
              case IntValue(n) =>
                val tmp = rpool.get()
                mips("li", tmp + ", " + n)
                mips("move", "$a0, " + tmp)
                mips("li", "$v0, 1")
                mips("syscall")
                rpool.recycle(tmp)

              case u =>
                val v = emit(u)
                mips("move", "$a0, " + v)
                mips("li", "$v0, 1")
                mips("syscall")
                rpool.recycle(v)
            }

          case "READ_INT" =>
            value match {
              case Mem(Binop("PLUS", Reg(r), IntValue(n))) =>
                val t1 = rpool.get()
                val t2 = rpool.get()
                mips("li", t1 + ", " + n)
                mips("addu", t2 + ", " + emit(Reg(r)) + ", " + t1)
                mips("li", "$v0, 5")
                mips("syscall")
                mips("sw", "$v0, (" + t2 + ")")
                rpool.recycle(t1)
                rpool.recycle(t2)
                
              case _ =>
                val v = emit(value)
                mips("li", "$v0, 5")
                mips("syscall")
                mips("sw", "$v0, (" + v + ")")
                rpool.recycle(v)
            }
        }

      case Return() =>
        mips("jr", "$ra")

      case Jump(label) =>
        mips("j", label)

      case _ => throw new Error("*** Unknown IR " + e)
    }
  }

  /** generate initial MIPS code from the program */
  def initialCode() {
    mips(".globl", "main")
    mips(".data")
    mips_label("ENDL_")
    mips(".asciiz", "\"\\n\"")
    mips(".text")
  }
}
