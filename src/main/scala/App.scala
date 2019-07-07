package automata
object App {
  def main(args: Array[String]): Unit = {
    Commands.loop().unsafeRun()
  }
}
