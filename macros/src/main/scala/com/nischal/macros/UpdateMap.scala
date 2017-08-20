package com.nischal.macros

import scala.collection.immutable.Seq
import scala.meta._

class UpdateMap(debug: Boolean = false) extends scala.annotation.StaticAnnotation
{

  inline def apply(defn: Any): Any = meta {
    //get the arguments
    val debug = this match {
      // The argument needs to be a literal like `1` or a string like `"foobar"`.
      case q"new $_(${Lit.Boolean(ap)})" => ap
      case _ => false // default value
    }

    defn match {
      case cls@Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template) =>
        //generate the update sql function
        val updateSQL = SQLGeneratorMacro.generateUpdateSQL(paramss.flatten)
        //append it to the class functions
        val templateStats: Seq[Stat] = updateSQL +: template.stats.getOrElse(Nil)
        //create new class with the update sql function
        val newClass = cls.copy(templ = template.copy(stats = Some(templateStats)))
        //debug
        if (debug) Helper.debug(newClass)

        newClass
      case Term.Block(
      Seq(cls@Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template), companion: Defn.Object)
      ) =>
        //generate the update sql function
        val updateSQL = SQLGeneratorMacro.generateUpdateSQL(paramss.flatten)
        //append it to the class functions
        val templateStats: Seq[Stat] = updateSQL +: template.stats.getOrElse(Nil)
        //create new class with the update sql function
        val newClass = cls.copy(templ = template.copy(stats = Some(templateStats)))
        
        val newClassWithCompanion = Term.Block(Seq(newClass, companion))
        //debug
        if (debug) Helper.debug(newClassWithCompanion)

        newClassWithCompanion
      case _ =>
        Helper.debug(defn.structure)
        abort("@Class2Map must annotate a class.")
    }
  }
}