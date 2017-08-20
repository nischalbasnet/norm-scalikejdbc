## Macro annotations to add orm functionality to case class 

### Example
```scala
@UpdateMap
case class UserUpdate(
  first_name: Option[String],
  middle_name: Option[String],
  last_name: Option[String],
  age: Option[Int]
)
```
Produces:
```scala
case class UserUpdate(
first_name: Option[String],
middle_name: Option[String],
last_name: Option[String],
age: Option[Int]) {

  def updateSQL(nullables: Seq[String] = Seq.empty): _root_.scala.collection.immutable.Map[String, scalikejdbc.SQLSyntax] = {
    val updateMap = _root_.scala.collection.mutable.Map[String, scalikejdbc.SQLSyntax]()
    if (first_name.isDefined) updateMap.put("first_name", scalikejdbc.nischalmod.SQLBinder.bind(first_name.get)) 
    else if (nullables.contains("first_name")) updateMap.put("first_name", scalikejdbc.nischalmod.SQLBinder.bind(null))
    if (middle_name.isDefined) updateMap.put("middle_name", scalikejdbc.nischalmod.SQLBinder.bind(middle_name.get)) 
    else if (nullables.contains("middle_name")) updateMap.put("middle_name", scalikejdbc.nischalmod.SQLBinder.bind(null))
    if (last_name.isDefined) updateMap.put("last_name", scalikejdbc.nischalmod.SQLBinder.bind(last_name.get)) 
    else if (nullables.contains("last_name")) updateMap.put("last_name", scalikejdbc.nischalmod.SQLBinder.bind(null))
    if (age.isDefined) updateMap.put("age", scalikejdbc.nischalmod.SQLBinder.bind(age.get)) 
    else if (nullables.contains("age")) updateMap.put("age", scalikejdbc.nischalmod.SQLBinder.bind(null))
    
    updateMap.toMap
  }
}
```
```scala
@NormModel
case class Person(
  id: String,
  @update first_name: String,
  @update middle_name: Option[String],
  @update last_name: String,
  @update email: String,
  @update age: Option[Int]
) extends INormModel
```
Produces
```scala
case class Person(
id: String,
@update first_name: String,
@update middle_name: Option[String],
@update last_name: String,
@update email: String,
@update age: Option[Int]
) extends INormModel() {

  private val _updateForm = Person.Update()
  
  def insertSQL(exclude: Seq[String] = Seq.empty): _root_.scala.collection.immutable.Map[String, scalikejdbc.SQLSyntax] = {
    val insertMap = _root_.scala.collection.mutable.Map[String, scalikejdbc.SQLSyntax]()
    if (!exclude.contains("id")) insertMap.put("id", scalikejdbc.nischalmod.SQLBinder.bind(id))
    if (!exclude.contains("first_name")) insertMap.put("first_name", scalikejdbc.nischalmod.SQLBinder.bind(first_name))
    if (!exclude.contains("middle_name")) insertMap.put("middle_name", scalikejdbc.nischalmod.SQLBinder.bind(middle_name))
    if (!exclude.contains("last_name")) insertMap.put("last_name", scalikejdbc.nischalmod.SQLBinder.bind(last_name))
    if (!exclude.contains("email")) insertMap.put("email", scalikejdbc.nischalmod.SQLBinder.bind(email))
    if (!exclude.contains("age")) insertMap.put("age", scalikejdbc.nischalmod.SQLBinder.bind(age))
    insertMap.toMap
  }
  
  def updateSQL() = _updateForm.updateSQL()
  
  def setFirstName(first_name: String, setNull: Boolean = false) = {
    if (setNull) _updateForm.setNullValue("first_name") else _updateForm.first_name = Some(first_name)
    this
  }
  def setMiddleName(middle_name: String, setNull: Boolean = false) = {
    if (setNull) _updateForm.setNullValue("middle_name") else _updateForm.middle_name = Some(middle_name)
    this
  }
  def setLastName(last_name: String, setNull: Boolean = false) = {
    if (setNull) _updateForm.setNullValue("last_name") else _updateForm.last_name = Some(last_name)
    this
  }
  def setEmail(email: String, setNull: Boolean = false) = {
    if (setNull) _updateForm.setNullValue("email") else _updateForm.email = Some(email)
    this
  }
  def setAge(age: Int, setNull: Boolean = false) = {
    if (setNull) _updateForm.setNullValue("age") else _updateForm.age = Some(age)
    this
  }
}
```

#### Note: Intellij has issue expanding the NormModel annotation. And ouputs were formatted later and is not exactly how it looks in console.
