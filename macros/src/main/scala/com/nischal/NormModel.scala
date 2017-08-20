package com.nischal

import scala.meta._

class update extends scala.annotation.StaticAnnotation

class NormModel extends scala.annotation.StaticAnnotation
{
  inline def apply(defn: Any): Any = meta {
    import scala.collection.immutable.Seq
    val (cls, companion) = defn match {
      case q"${cls: Defn.Class}; ${companion: Defn.Object}" =>
        (cls, companion)
      case cls: Defn.Class =>
        (cls, q"object ${Term.Name(cls.name.value)}")
      case _ =>
        println(defn.structure)
        abort("@NormModel must annotate a class.")
    }

    val allParams = cls.ctor.paramss.flatten
    val insertSql = SQLGeneratorMacro.generateInsertSQL(allParams)
    val updateSetters = GenericMacro.generateUpdateSetters(allParams)
    val updateForm = q"""private val _updateForm = ${Term.Name(cls.name.value)}.Update()"""
    val updateSql = q"def updateSQL() = _updateForm.updateSQL()"

    val templateStats: Seq[Stat] = (updateForm +: insertSql +: updateSql +: cls.templ.stats.getOrElse(Nil)) ++ updateSetters
    val newClass = cls.copy(templ = cls.templ.copy(stats = Some(templateStats)))

    //work on the companion objects
    val ormModels = SQLGeneratorMacro.generateAnnotatedClass(allParams)
    val newCompanion = companion.copy(
      templ = companion.templ.copy(
        stats = Some(companion.templ.stats.getOrElse(Nil) ++ ormModels)
      )
    )
    println(newClass)
    println(newCompanion)
    println("End from orm macro")

    Term.Block(Seq(newClass, newCompanion))
  }
}
