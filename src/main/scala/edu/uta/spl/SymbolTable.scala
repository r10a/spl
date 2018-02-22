package edu.uta.spl

sealed abstract class Declaration
/** type declaration: has a type */
case class TypeDeclaration ( hastype: Type ) extends Declaration
/** variable declaration: the type and the level/offset of a variable in a frame */
case class VarDeclaration ( vartype: Type, level: Int, offset: Int ) extends Declaration
/** function declaration: output type, formal parameters, code address, next available offset in a frame */
case class FuncDeclaration ( outtype: Type, params: List[Bind[Type]],
                             label: String, level: Int, available_offset: Int ) extends Declaration


/** A symbol table to store PCAT declarations */
class SymbolTable {
  var symbol_table: List[List[(String,Declaration)]] = Nil

  /** true if the item exists in the symbol table */
  def exists ( key: String ): Boolean = {
    val ds = for ( s <- symbol_table;
                   (n,d) <- s if n.equals(key)
                 ) yield d
    ds == Nil
  }

  /** lookup for an item in the symbol table */
  def lookup ( key: String ): Option[Declaration] = {
    val ds = for ( s <- symbol_table;
                   (n,d) <- s if n.equals(key)
                 ) yield d
    ds match {
      case c::cs => Some(c)
      case _ => None
    }
  }

  /** insert a new item in the symbol table */
  def insert ( key: String, declaration: Declaration ) {
    symbol_table match {
      case c::cs => symbol_table = ((key,declaration)::c)::cs
      case _ => throw new Error("Empty scope")
    }
  }

  /** replace an existing item in the symbol table */
  def replace ( key: String, declaration: Declaration ) {
    symbol_table = symbol_table.map(_.map( b => if (b._1.equals(key))
                                                   (key,declaration)
                                                else b ))
  }

  /** start a new scope */
  def begin_scope () {
    symbol_table = List()::symbol_table
  }

  /** pop the last scope */
  def end_scope () {
    symbol_table match {
      case c::cs => symbol_table = cs
      case _ => throw new Error("Empty scope")
    }
  }

  override def toString: String = {
    symbol_table.toString
  }

}
