package automata

import java.io.{FileOutputStream, ObjectOutputStream}

import scala.annotation.tailrec

object types {
type DeltaFunction = Map[(State, Letter), State]
}

case class State(state: String) {
  def nonEmpty: Boolean = state.nonEmpty
}

case class Letter(letter: String)

case class Automata(name: String,
                    states: Set[State], alphabet: Set[Letter],
                    startState: State, finalStates: Set[State],
                    deltaFunction: types.DeltaFunction) extends Serializable {
  def isWordRecognized(word: List[Letter]): Boolean = traverseWord(word) match {
    case Success(_) => true
    case Failure(_, _) => false
    case _ => sys.error("Unexpected return type of traverseWord. ")
  }

  def traverseWord(word: List[Letter]): Checked[String, State] = {
    @tailrec
    def helper(currState: State, currLetterIndex: Int, path: String): Checked[String, State] = {
      val currLetter = word(currLetterIndex)

      //next letter exists
      if (currLetterIndex + 1 == word.length) {
        if (isTransitionPossible(currState, currLetter)) {
          helper(findNextState(currState, currLetter), currLetterIndex + 1,
            path ++ currState.state ++ currLetter.toString)
        }
        else {
          return Failure(path, currState)
        }
      }
      //there are no letters left
      else {
        if (finalStates.contains(currState)) {
          return Success(path ++ currState.state)
        }
        else {
          return Failure(path ++ currState.state, currState)
        }
      }
    }
    helper(startState, 0, "")
  }

  def isTransitionPossible(fromState: State, withLetter: Letter): Boolean = {
    val possibleStates = deltaFunction.get((fromState, withLetter))

    possibleStates match {
      case Some(_) => true
      case _ => false
    }
  }

  def findNextState(fromState: State, withLetter: Letter): State = {
    val possibleStates = deltaFunction.get((fromState, withLetter))

    possibleStates match {
      case Some(s) => s
      case _ => sys.error("Wrong way of calling function findNextState. " +
        "It must be called only when it is sure there is next state. ")
    }
  }
}
