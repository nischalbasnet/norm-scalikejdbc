package com.nischal

import scala.collection.immutable.Seq
import scala.meta._

object GenericMacro
{

  def generateToMap(params: Seq[Term.Param]): Defn.Def =
  {
    val namesToValues: Seq[Term.Tuple] = params.map { param =>
      val syntax = s"${param.name.value}"
      val value = Term.Name(param.name.value)
      q"""($syntax, $value)"""
    }
    val toMapImpl: Term = q"Map(..$namesToValues)"

    q"""def toMap: Map[String, Any] = $toMapImpl"""
  }

  def generateApply(name: Type.Name, paramss: Seq[Seq[Term.Param]]): Defn.Def =
  {
    val args = paramss.map(_.map(param => Term.Name(param.name.value)))
    q"""def apply(...$paramss): $name = new ${Ctor.Ref.Name(name.value)}(...$args)"""
  }

  def generateUpdateSetters(params: Seq[Term.Param]): Seq[Defn.Def] =
  {
    import scala.collection.mutable
    val paramsWithAnnotation = for {
      param <- params
      seenMods = mutable.Set.empty[String]
      modifier <- param.mods if seenMods.add(modifier.toString)
      newParam <- modifier match {
        case mod"@update" =>
          param.decltpe collect {
            case tpe: Type.Name =>
              (param"${param.name}: Option[$tpe]", param"${param.name}: $tpe")
            case Type.Apply(Type.Name(tpeName), wrappedTpe :: Nil) if tpeName == "Option" =>
              (param"${param.name}: Option[$wrappedTpe]", param"${param.name}: $wrappedTpe")
          }
        case _ => None
      }
    } yield newParam

    val updateSetters = paramsWithAnnotation.map(p => generateUpdateSetter(p._1, p._2))
    updateSetters
  }

  def generateUpdateSetter(param: Term.Param, nonOptionParam: Term.Param): Defn.Def =
  {
    val camelCaseParam = param.name.value.split("_")
      .map(_.capitalize)
      .mkString("")

    val methodName = Term.Name(s"set$camelCaseParam")

    q"""def $methodName($nonOptionParam, setNull: Boolean = false) = {
          if(setNull) _updateForm.setNullValue(${param.name.value})
          else _updateForm.${Term.Name(param.name.value)} = Some(${Term.Name(nonOptionParam.name.value)})

          this
       }
     """
  }
}
