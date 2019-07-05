sealed trait Checked[+P, +F]

case class Success[+P](path: P) extends Checked[P, Nothing]
case class Failure[+P, +F](path: P, failureState: F) extends Checked[P, F]

