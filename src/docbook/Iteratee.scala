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