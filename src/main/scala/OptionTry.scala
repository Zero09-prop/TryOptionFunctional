import scala.util.control.NonFatal
sealed abstract class OptionTry[+T] {
  def map[U](f: T => U): OptionTry[U]
  def flatMap[U](f: T => OptionTry[U]): OptionTry[U]
  def get: T
  final def isDefined: Boolean = !isEmpty
  final def isEmpty: Boolean = this eq Nones
  def foreach[U](f: T => U): Unit = if (!isEmpty) f(this.get)
  def flatten[U](): OptionTry[U]
}
object OptionTry {
  def apply[T](code: => T): OptionTry[T] = {
    try {
      val codeRun = code
      if (codeRun == null) Nones
      else SuccesS(codeRun)
    } catch {
      case NonFatal(e) => FailureS(e)
    }
  }
}
final case class SuccesS[+T](value: T) extends OptionTry[T] {
  override def get: T = value
  override def map[U](f: T => U): OptionTry[U] = OptionTry(f(value))
  override def flatMap[U](f: T => OptionTry[U]): OptionTry[U] =
    try { f(value) }
    catch {
      case NonFatal(e) => FailureS(e)
    }
  override def foreach[U](f: T => U): Unit = f(value)
  override def flatten[U](): OptionTry[U] = this.asInstanceOf[OptionTry[U]]
}
final case class FailureS[+T](exception: Throwable) extends OptionTry[T] {
  override def get: T = throw exception
  override def map[U](f: T => U): OptionTry[U] = this.asInstanceOf[OptionTry[U]]
  override def flatMap[U](f: T => OptionTry[U]): OptionTry[U] = this.asInstanceOf[OptionTry[U]]
  override def foreach[U](f: T => U): Unit = ()
  override def flatten[U](): OptionTry[U] = this.asInstanceOf[OptionTry[U]]
}
final case object Nones extends OptionTry[Nothing] {
  override def get: Nothing = throw new NoSuchElementException("Nones.get")
  override def map[U](f: Nothing => U): OptionTry[U] = this.asInstanceOf[OptionTry[U]]
  override def flatMap[U](f: Nothing => OptionTry[U]): OptionTry[U] = this.asInstanceOf[OptionTry[U]]
  override def flatten[U](): OptionTry[U] = this.asInstanceOf[OptionTry[U]]
}
