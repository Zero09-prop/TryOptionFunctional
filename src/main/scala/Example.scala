import scala.util.Try

class Person(
    val lastName: String,
    val firstName: String,
    private val patronymic: String
) {
  def Patron(): String = {
    Option(patronymic) match {
      case Some(some) => some
      case None       => "Patron don't exist"
    }
  }
}

object Example extends App {
  val person1 = new Person("Popov", "Misha", null)
  val person2 = new Person("Abrah", "Flop", "Flip")

  println(person1.Patron())
  println(person2.Patron())

  val array = Array(1, 2, 3, 0, 7, 3, 4)
  val number = Try(array.reduce((x, y) => x / y))
  number.foreach(println)
  val str: String = "null"
  Try(5 / 0).map(_ * 2)
  println(OptionTry(str).flatten())

}
