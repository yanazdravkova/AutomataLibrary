package automata

import scala.io.Source
import Serializer.convertToFileName

object Deserializer {

  def deserialize(automataName: String): Automata = {
    val buffSource = Source.fromFile(convertToFileName(automataName))
    val lines = buffSource.getLines.toList
    val states = lines(1)
    val alphabet = lines(2)
    val startState = lines(3)
    val finalStates = lines(4)
    val sizeOfDeltaFunction = lines(5).toInt
    val deltaFunction = (for(i <- 6 to (sizeOfDeltaFunction + 5)) yield lines(i).split(" ")).toList

    new Automata(automataName,deserializeStates(states), deserializeLetters(alphabet),
      State(startState), deserializeStates(finalStates), deserializeDeltaFunction(deltaFunction))
  }

  def deserializeStates(states: String): Set[State] = {
    val splittedStates = states.split(" ")
    (for (s <- splittedStates) yield State(s)).toSet
  }

  def deserializeLetters(alphabet: String): Set[Letter] = {
    val splittedLetters = alphabet.split(" ")
    (for (l <- splittedLetters) yield Letter(l)).toSet
  }

  def deserializeDeltaFunction(deltaFunction: List[Array[String]]) = {
    def convertArrayOfStringsToRule(ruleString: Array[String]) =
      ((State(ruleString(0)), Letter(ruleString(1))), State(ruleString(2)))
    deltaFunction.map(x => convertArrayOfStringsToRule(x)).toMap
  }
}
