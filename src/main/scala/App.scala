package automata
object App {
  def main(args: Array[String]): Unit = {
    /*val serializer = new Serializer
    AutomataRegistrationApp.loop(serializer).unsafeRun()
*/
    /* val a1 = new Automata("test", Set(State("1"), State("2"), State("3"), State("4")), Set(Letter("a"), Letter("b"), Letter("c")),
       State("1"), Set(State("3"), State("4")), Map((State("1"), Letter("a")) -> State("2"), (State("2"), Letter("b")) -> State("3")))

     val a2 = new Automata("aut", Set(State("1"), State("2"), State("5"), State("4")), Set(Letter("a"), Letter("b"), Letter("c")),
       State("1"), Set(State("5"), State("4")), Map((State("1"), Letter("a")) -> State("2"), (State("2"), Letter("b")) -> State("3")))

     Serializer.serialize(a2)
    val deserializedAutomata = Deserializer.deserialize("test")
    AutomataRegistrationApp.print(deserializedAutomata).unsafeRun()
    //s.serialize(a2)
     */
   // AutomataOperationsApp.traverseWord.unsafeRun()
    AutomataOperationsApp.toDotFormat.unsafeRun()
  }
}
