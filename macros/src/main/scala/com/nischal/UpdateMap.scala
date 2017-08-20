package com.nischal

import scala.collection.immutable.Seq
import scala.meta._

class UpdateMap extends scala.annotation.StaticAnnotation
{

  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls@Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template) =>
        //generate the update sql function
        val updateSQL = SQLGeneratorMacro.generateUpdateSQL(paramss.flatten)
        //append it to the class functions
        val templateStats: Seq[Stat] = updateSQL +: template.stats.getOrElse(Nil)
        //create new class with the update sql function
        cls.copy(templ = template.copy(stats = Some(templateStats)))
      case Term.Block(
      Seq(cls@Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template), companion: Defn.Object)
      ) =>
        //generate the update sql function
        val updateSQL = SQLGeneratorMacro.generateUpdateSQL(paramss.flatten)
        //append it to the class functions
        val templateStats: Seq[Stat] = updateSQL +: template.stats.getOrElse(Nil)
        //create new class with the update sql function
        val newClass = cls.copy(templ = template.copy(stats = Some(templateStats)))

        Term.Block(Seq(newClass, companion))
      case _ =>
        println(defn.structure)
        abort("@Class2Map must annotate a class.")
    }
  }
}

