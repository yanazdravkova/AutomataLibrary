package automata
object App {
  def main(args: Array[String]): Unit = {
    AutomataRegistrationApp.loopRegistratioOfAutomata.unsafeRun()
    //AutomataOperationsApp.traverseWord.unsafeRun()
    //AutomataOperationsApp.toDotFormat.unsafeRun()
  }
}
