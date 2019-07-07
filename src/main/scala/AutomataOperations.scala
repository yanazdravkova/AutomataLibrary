package automata
object AutomataOperations {
def traverseWord(automataName: String, word: List[String]): Checked[String, State] = {
  val automata = Deserializer.deserialize(automataName)
  automata.traverseWord(word)
}
}
