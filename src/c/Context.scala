package c

import scala.collection.mutable

class Context {
  val typedefs = mutable.Map[String, Type]()
  val structs = mutable.Map[String, Type]()
  val enums = mutable.Map[String, Type]()
  val unions = mutable.Map[String, Type]()

  val variables = mutable.Map[String, (Type, Option[Expr])]()
  val functions = mutable.Map[String, (Type, Array[Param], Option[Block])]()

  def isType(name: String) = typedefs contains name
  def isVariable(name: String) = variables contains name
  def isFunction(name: String) = functions contains name

  def TypeDef(typ: Type, name: String) = {
    typedefs(name) = typ
    new TypeDef(typ, name)
  }

  def StructDef(name: String, fields: Array[Field]) = {
    structs(name) = StructType(fields)
    new StructDef(name, fields)
  }

  def UnionDef(name: String, fields: Array[Field]) = {
    unions(name) = UnionType(fields)
    new UnionDef(name, fields)
  }

  def EnumDef(name: String, consts: Array[String]) = {
    enums(name) = EnumType(consts)
    new EnumDef(name, consts)
  }

  def VarDef(typ: Type, name: String) = {
    variables(name) = (typ, None)
    new VarDef(typ, name, None)
  }

  def VarDef(typ: Type, name: String, init: Expr) = {
    variables(name) = (typ, Some(init))
    new VarDef(typ, name, Some(init))
  }
  
  def FunDef(ret: Type, name: String, params: Array[Param]) = {
    functions(name) = (ret, params, None)
    new FunDef(ret, name, params, None)
  }
  
  def FunDef(ret: Type, name: String, params: Array[Param], body: Block) = {
    functions(name) = (ret, params, Some(body))
    new FunDef(ret, name, params, Some(body))
  }
}