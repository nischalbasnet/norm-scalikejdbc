package com.nischal.macros

import scala.meta._

class update extends scala.annotation.StaticAnnotation

class NormModel(debug: Boolean = false) extends scala.annotation.StaticAnnotation
{
  inline def apply(defn: Any): Any = meta {
    import scala.collection.immutable.Seq
    //get the arguments
    val debug = this match {
      // The argument needs to be a literal like `1` or a string like `"foobar"`.
      case q"new $_(${Lit.Boolean(ap)})" => ap
      case _ => false // default value
    }

    val (cls, companion) = defn match {
      case q"${cls: Defn.Class}; ${companion: Defn.Object}" =>
        (cls, companion)
      case cls: Defn.Class =>
        (cls, q"object ${Term.Name(cls.name.value)}")
      case _ =>
        Helper.debug(defn.structure)
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

    if(debug){
      Helper.debug(newClass)
      Helper.debug(newCompanion)
      Helper.debug("End from orm macro")
    }

    Term.Block(Seq(newClass, newCompanion))
  }
}
