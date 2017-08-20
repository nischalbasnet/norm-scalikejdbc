package com.nischal.macros

import scala.collection.immutable.Seq
import scala.meta._

/**
  * Created by nbasnet on 6/24/17.
  */
class ClassToMap(arg: Boolean = false) extends scala.annotation.StaticAnnotation
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
        val toMap = GenericMacro.generateToMap(paramss.flatten)

        val templateStats: Seq[Stat] = toMap +: template.stats.getOrElse(Nil)
        val newClass = cls.copy(templ = template.copy(stats = Some(templateStats)))

        //debug
        if (debug) Helper.debug(newClass)
        newClass
      case Term.Block(
      Seq(cls@Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template), companion: Defn.Object)
      ) =>
        val toMap = GenericMacro.generateToMap(paramss.flatten)

        val templateStats: Seq[Stat] = toMap +: template.stats.getOrElse(Nil)
        val newClass = cls.copy(templ = template.copy(stats = Some(templateStats)))

        val newClassWithCompanion = Term.Block(Seq(newClass, companion))

        //debug
        if (debug) {
          Helper.debug(newClass)
        }
        newClassWithCompanion
      case _ =>
        Helper.debug(defn.structure)
        abort("@Class2Map must annotate a class.")
    }
  }
}
