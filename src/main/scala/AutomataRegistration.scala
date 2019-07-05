
object AutomataRegistration {
  def registerAutomata(name: String,
                       states: Set[String], alphabet: Set[Char],
                       startState: String, finalStates: Set[String],
                       deltaFunction: Map[(String, Char), String]) = {
    import AutomataValidation._
    (
      validateName(),
      validateStatesSet(),
      validateAlphabet(),
      validateStartState(),
      validateFinalStatesSet(),
      validateDeltaFunction(),
    )
  }
}
sealed trait RegistrationError

case object NameIsEmpty extends  RegistrationError

case object StatesSetIsEmpty extends RegistrationError

case object AlphabetIsEmpty extends RegistrationError

case object StartStateIsEmpty extends RegistrationError
case object StartStateNotPartOfStatesSet extends RegistrationError

case object FinalStatesSetIsEmpty extends RegistrationError
case object FinalStatesNotSubsetOfAllStates extends RegistrationError

case object DeltaFunctionIsEmpty extends RegistrationError
case object DeltaFunctionIsIncompatible extends RegistrationError

object AutomataValidation {
  //!!! TO DO Check if automata with the same name is already saved in the file
  def validateName(name: String):Validated[RegistrationError, String] = {
    if(name.nonEmpty) Valid(name)
    else Invalid(Chain(NameIsEmpty))
  }

  def validateStatesSet(states: Set[String]):Validated[RegistrationError, Set[String]] = {
    if(states.nonEmpty) Valid(states)
    else Invalid(Chain(StatesSetIsEmpty))
  }

  def validateAlphabet(alphabet: Set[Char]):Validated[RegistrationError, Set[Char]] = {
    if(alphabet.nonEmpty) Valid(alphabet)
    else Invalid(Chain(AlphabetIsEmpty))
  }

  def validateStartState(startState: String, states: Set[String]):Validated[RegistrationError, String] = {
   val validateStartStateIsNotEmpty =
     if(startState.nonEmpty) Valid(startState)
     else Invalid(Chain(StartStateIsEmpty))

    val validateStartStateIsPartOfStates =
     if(states.contains(startState)) Valid(startState)
     else Invalid(Chain(StartStateNotPartOfStatesSet))

    (
      validateStartStateIsNotEmpty,
      validateStartStateIsPartOfStates
    ).zip.map(_ => startState)
  }

  def validateFinalStatesSet(finalStates: Set[String], states: Set[String]):Validated[RegistrationError, Set[String]]  = {
    val validateFinalStatesSetIsNotEmpty =
      if(finalStates.nonEmpty) Valid(finalStates)
      else Invalid(Chain(FinalStatesSetIsEmpty))

    val validateFinalStatesSetIsSubsetOfAllStates =
      if(finalStates.subsetOf(states)) Valid(finalStates)
      else Invalid(Chain(FinalStatesNotSubsetOfAllStates))

    (
      validateFinalStatesSetIsNotEmpty,
      validateFinalStatesSetIsSubsetOfAllStates
    ).zip.map(_ => finalStates)
  }

  def validateDeltaFunction(deltaFunction: Map[(String, Char), String],
                            states: Set[String], alphabet: Set[Char]): Validated[RegistrationError, Map[(String, Char), String]]  = ???
  /*{
    val validateDeltaFunctionIsNotEmpty =
      if(deltaFunction.nonEmpty) Valid(deltaFunction)
      else Invalid(Chain(DeltaFunctionIsEmpty))

    val validateDeltaFunctionCompatibility =
      for((key, value) <- deltaFunction) {
        if(!(states.contains(key._1) && alphabet.contains(key._2) && states.contains(value))) Invalid(Chain(DeltaFunctionIsIncompatible))
      }
    Valid(deltaFunction)

    (
      validateDeltaFunctionIsNotEmpty,
      validateDeltaFunctionCompatibility
    ).zip.map((=> deltaFunction)
  }*/
}