import scala.annotation.tailrec

case class Automata(name: String,
                    states: Set[String], alphabet: Set[Char],
                    startState: String, finalStates: Set[String],
                    deltaFunction: Map[(String, Char), String]) {
  def isWordRecognized(word: String): Boolean = traverseWord(word) match {
    case Success(_) => true
    case Failure(_, _) => false
    case _ => sys.error("Unexpected return type of traverseWord. ")
  }

  def traverseWord(word: String): Checked[String, String] = {
    val listOfLetters = word.toList
    @tailrec
    def helper(currState: String, currLetterIndex: Int, path: String): Checked[String, String] = {
      val currLetter = listOfLetters(currLetterIndex)

      //next letter exists
      if (currLetterIndex + 1 == listOfLetters.length) {
        if (isTransitionPossible(currState, currLetter)) {
          helper(findNextState(currState, currLetter), currLetterIndex + 1,
            path ++ currState ++ currLetter.toString)
        }
        else {
          return Failure(path, currState)
        }
      }
      //there are no letters left
      else {
        if (finalStates.contains(currState)) {
          return Success(path ++ currState)
        }
        else {
          return Failure(path ++ currState, currState)
        }
      }
    }
    helper(startState, 0, "")
  }

  def isTransitionPossible(fromState: String, withLetter: Char): Boolean = {
    val possibleStates = deltaFunction.get((fromState, withLetter))

    possibleStates match {
      case Some(_) => true
      case _ => false
    }
  }

  def findNextState(fromState: String, withLetter: Char): String = {
    val possibleStates = deltaFunction.get((fromState, withLetter))

    possibleStates match {
      case Some(s) => s
      case _ => sys.error("Wrong way of calling function findNextState. " +
        "It must be called only when it is sure there is next state. ")
    }

  }
}

/*object Automaton{
  def apply() = ???
}*/