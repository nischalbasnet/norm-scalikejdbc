package scalikejdbc.nischalmod

object SqlHelpers
{

}

object SQLBinder
{

  import scalikejdbc._

  def bind[T](value: T): scalikejdbc.SQLSyntax = sqls"$value"
}
