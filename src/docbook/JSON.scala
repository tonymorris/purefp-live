sealed trait JSON
case class JBool(b: Boolean) extends JSON
case class JNumber(n: Double) extends JSON
case class JString(s: String) extends JSON
case object JNull extends JSON
case class JArray(x: List[JSON]) extends JSON
case class JObject(x: List[(String, JSON)]) extends JSON

case class @?>[A, B](f: A => Option[(B => A, B)]) {
  def >=>[C](g: B @?> C): A @?> C =
    @?>(a => f(a) flatMap {
      case (q, r) => g f r map {
        case (s, t) => (q compose s, t)
      }
    })
}

object JSON {
  val jBoolL: JSON @?> Boolean =
    @?> {
      case JBool(b) => Some((JBool(_), b))
      case _ => None
    }

  val jNumberL: JSON @?> Double =
    @?> {
      case JNumber(n) => Some((JNumber(_), n))
      case _ => None
    }

  val jArrayL: JSON @?> List[JSON] =
    @?> {
      case JArray(x) => Some((JArray(_), x))
      case _ => None
    }

  val jObjectL: JSON @?> List[(String, JSON)] =
    @?> {
      case JObject(x) => Some((JObject(_), x))
      case _ => None
    }
}