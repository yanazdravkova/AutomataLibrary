package automata

import java.io.{FileOutputStream, ObjectOutputStream}

import scala.annotation.tailrec

object types {
type DeltaFunction = Map[(State, Letter), State]
  type Path = List[(State, Letter, State)]
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

  def traverseWord(word: List[Letter]): Checked[types.Path, State] = {
    ///TODO Refactor - to be more readable
    @tailrec
    def helper(currState: State, currLetterIndex: Int, path: types.Path): Checked[types.Path, State] = {
      val currLetter = word(currLetterIndex)

      //next letter exists
      if ((currLetterIndex + 1 )!= word.length) {
        if (isTransitionPossible(currState, currLetter)) {
          val nextState = findNextState(currState, currLetter)
          helper(nextState, currLetterIndex + 1,
            path.appended((currState, currLetter, nextState)))
        }
        else {
          return Failure(path, State(currState.state))
        }
      }
      //there are no letters left
      else {
        if(isTransitionPossible(currState, currLetter)){
          val nextState = findNextState(currState, currLetter)
          if (finalStates.contains(nextState)) return Success(path.appended((currState, currLetter, nextState)))
          else Failure(path, currState)
        }
        else {
          return Failure(path, currState)
        }
      }
    }
    helper(startState, 0, List.empty)
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

  def toDotFormat(): String = {
    "digraph {" +
    deltaFunction.foldLeft("") { (s: String, pair: ((State, Letter), State)) =>
      s + pair._1._1.state + "->"  + pair._2.state + "[label=\"" +  pair._1._2.letter +  "\"];" + "\r\n"} +
      startState.state + "[color = yellow];" + "\r\n" +
      finalStates.foldLeft("") { (s: String, state: State) =>  state.state + "[color = green];" + "\r\n"} +
      "}"
  }
}
