sealed trait Input[E]
case class Empty[E]() extends Input[E]
case class Eof[E]() extends Input[E]
case class El[E](e: E) extends Input[E]

// that which is being iterated
sealed trait Iteratee[E, A]
case class Done[E, A](a: A, r: Input[E])
    extends Iteratee[E, A]
case class Cont[E, A](k: Input[E] => Iteratee[E, A])
    extends Iteratee[E, A]

object Iteratee {
  def peek[E]: Iteratee[E, Option[E]] = {
    def step(s: Input[E]): Iteratee[E, Option[E]] =
      s match {
        case Empty() => Cont(step)
        case Eof()   => Done(None, Eof())
        case El(k)   => Done(Some(k), Eof())
      }
    Cont(step)
  }

  def drop[E](n: Int): Iteratee[E, Unit] = {
    def step(s: Input[E]): Iteratee[E, Unit] =
      s match {
        case Empty() => Cont(step)
        case Eof()   => Done((), Eof())
        case El(_)   => drop(n - 1)
      }
    if (n == 0) Done((), Empty())
    else Cont(step)
  }
}