package scalikejdbc.com.nischal

import scala.collection.immutable.{Seq, Map}
import scala.meta._
import scalikejdbc._

/**
  * Created by nbasnet on 6/24/17.
  */
class ClassToMap extends scala.annotation.StaticAnnotation
{
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls@Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template) =>
        val namesToValues: Seq[Term.Tuple] = paramss.flatten.map { param =>
          val syntax = s"${param.name.value}"
          val value = Term.Name(param.name.value)
          q"""($syntax, $value)"""
        }
        val toMapImpl: Term = q"Map(..$namesToValues)"

        val toMap =
          q"""def toMap: Map[String, Any] = $toMapImpl"""

        val namesToSyntax: Seq[Term.Tuple] = paramss.flatten.map { param =>
          val syntax = s"${param.name.value}"
          val value = Term.Name(param.name.value)
          q"""($syntax, scalikejdbc.nischalmod.SQLBinder.bind($value))"""
        }
        val insertSQLImpl = q"_root_.scala.collection.immutable.Map[String, scalikejdbc.SQLSyntax](..$namesToSyntax)"

        val insertSQL =
          q"""def insertSQL: Map[String, scalikejdbc.SQLSyntax] = $insertSQLImpl"""

        val templateStats: Seq[Stat] = toMap +: insertSQL +: template.stats.getOrElse(Nil)
        cls.copy(templ = template.copy(stats = Some(templateStats)))
      case _ =>
        println(defn.structure)
        abort("@Class2Map must annotate a class.")
    }
  }
}
