package examples

import com.nischal.{INormModel, NormModel, UpdateMap, update}
import scalikejdbc.com.nischal.ClassToMap

object MyApp
{
  def main(args: Array[String]): Unit =
  {
    scalikejdbcTest()
  }

  def scalikejdbcTest(): Unit =
  {
    val nischal = User("Nischal", None, "Basnet", 29)
    println("Hello Scala.meta macros! Scalikejdbc Test")
    println(nischal.insertSQL)


    val nischalup = UserUpdate(None, None, Some("Bas"), Some(19))
    println(nischalup.updateSQL(Seq("first_name")))

    println("ORM TESTING")
    val person: Person = Person("pid_111", "N1", None, "L1", "nn@outlook.com", Some(22))

    println(person.insertSQL())
    person.setFirstName("Jack")
    person.setMiddleName("", true)
    println(person.updateSQL())
    val updater = Person.Update(Some("test"))
    println(updater)
    println(updater.updateSQL())
  }
}

trait ClassExtensions
{
  //  def toMap: Map[String, Any]
  //  def toMap: Map[SQLSyntax, ParameterBinder]
}

@ClassToMap
case class User(
  first_name: String,
  middle_name: Option[String],
  last_name: String,
  age: Int
) extends ClassExtensions

case class Value[T](value: T)

@UpdateMap
case class UserUpdate(
  first_name: Option[String],
  middle_name: Option[String],
  last_name: Option[String],
  age: Option[Int]
)


@NormModel
case class Person(
  id: String,
  @update first_name: String,
  @update middle_name: Option[String],
  @update last_name: String,
  @update email: String,
  @update age: Option[Int]
) extends INormModel

object Person
{
  val t = ""
}