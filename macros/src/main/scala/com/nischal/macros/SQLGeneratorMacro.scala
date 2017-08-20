package com.nischal.macros

import scala.collection.immutable.Seq
import scala.meta._

object SQLGeneratorMacro
{

  def generateInsertSQL(params: Seq[Term.Param]): Defn.Def =
  {
    //    val namesToSyntax: Seq[Term.Tuple] = params.map { param =>
    //      val syntax = s"${param.name.value}"
    //      val value = Term.Name(param.name.value)
    //      q"""($syntax, scalikejdbc.nischalmod.SQLBinder.bind($value))"""
    //    }
    //    val insertSQLImpl = q"Map[String, scalikejdbc.SQLSyntax](..$namesToSyntax)"
    //    val insertSQL =
    //      q"""def insertSQL: _root_.scala.collection.immutable.Map[String, scalikejdbc.SQLSyntax] = $insertSQLImpl"""

    val insertMap = params.map { param =>
      val field = s"${param.name.value}"
      val value = Term.Name(param.name.value)
      q"""if(!exclude.contains($field)) insertMap.put($field, scalikejdbc.nischalmod.SQLBinder.bind($value))"""
    }

    val insertSQL =
      q"""def insertSQL(exclude: Seq[String] = Seq.empty): _root_.scala.collection.immutable.Map[String, scalikejdbc.SQLSyntax] = {
          val insertMap = _root_.scala.collection.mutable.Map[String, scalikejdbc.SQLSyntax]()
          ..$insertMap
          insertMap.toMap
        }
        """

    insertSQL
  }

  def generateUpdateSQL(params: Seq[Term.Param], defaultParam: Option[String] = None): Defn.Def =
  {
    val updateParts = params.map { param =>
      val field = s"${param.name.value}"
      val value = Term.Name(param.name.value)
      q"""if($value.isDefined) updateMap.put($field, scalikejdbc.nischalmod.SQLBinder.bind($value.get))
              else if(nullables.contains($field)) updateMap.put($field, scalikejdbc.nischalmod.SQLBinder.bind(null))"""
    }

    val default = defaultParam match {
      case Some(s: String) => s
      case _ => "Seq.empty"
    }
    val updateSQL =
      q"""def updateSQL(nullables: Seq[String] = ${Term.Name(default.toString())}): _root_.scala.collection.immutable.Map[String, scalikejdbc.SQLSyntax] = {
              val updateMap = _root_.scala.collection.mutable.Map[String, scalikejdbc.SQLSyntax]()
             ..$updateParts
             updateMap.toMap
            }"""

    updateSQL
  }

  def generateAnnotatedClass(params: Seq[Term.Param]): scala.Seq[Defn.Class] =
  {
    import scala.collection.mutable
    val paramsWithAnnotation = for {
      param <- params
      seenMods = mutable.Set.empty[String]
      modifier <- param.mods if seenMods.add(modifier.toString)
      newParam <- modifier match {
        case mod"@update" => param.decltpe collect {
          case tpe: Type.Name =>
            val defaultArg = param.default match {
              case Some(term) => q"Some($term)"
              case None => q"None"
            }
            param"var ${param.name}: Option[$tpe] = $defaultArg"
          case Type.Apply(Type.Name(tpeName), wrappedTpe :: Nil) if tpeName == "Option" =>
            val defaultArg = param.default match {
              case Some(term) => q"Some($term)"
              case None => q"None"
            }
            param"var ${param.name}: Option[$wrappedTpe] = $defaultArg"
        }
        case _ => None
      }
    } yield modifier -> newParam

    val grouped = paramsWithAnnotation
      .groupBy(_._1.toString)
      .mapValues(_.map(_._2))

    val models = grouped.map({
      case (annotation, classParams) =>
        val className = Type.Name(annotation.stripPrefix("@").capitalize)
        className.value match {
          case "Update" =>
            val classTemplate = generateUpdateSQL(classParams, Some("_nullValues"))

            q"""case class $className(..$classParams) extends com.nischal.norm.INormModelUpdater{
                private val _nullValues: _root_.scala.collection.mutable.ListBuffer[String] = _root_.scala.collection.mutable.ListBuffer()
                def setNullValue(value: String) = _nullValues.append(value)
                def setNullValue(values: Seq[String]) = _nullValues.appendAll(values)
                $classTemplate
              }"""
          case _ => q"case class $className(..$classParams)"
        }
    }).toSeq
    models
  }
}
