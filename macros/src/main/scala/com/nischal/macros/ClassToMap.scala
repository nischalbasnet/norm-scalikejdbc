package com.nischal.macros

import scala.collection.immutable.Seq
import scala.meta._

/**
  * Created by nbasnet on 6/24/17.
  */
class ClassToMap extends scala.annotation.StaticAnnotation
{
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls@Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template) =>
        val toMap = GenericMacro.generateToMap(paramss.flatten)

        val templateStats: Seq[Stat] = toMap +: template.stats.getOrElse(Nil)
        cls.copy(templ = template.copy(stats = Some(templateStats)))

      case Term.Block(
      Seq(cls@Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template), companion: Defn.Object)
      ) =>
        val toMap = GenericMacro.generateToMap(paramss.flatten)

        val templateStats: Seq[Stat] = toMap +: template.stats.getOrElse(Nil)
        val newClass = cls.copy(templ = template.copy(stats = Some(templateStats)))

        Term.Block(Seq(newClass, companion))
      case _ =>
        println(defn.structure)
        abort("@Class2Map must annotate a class.")
    }
  }
}
