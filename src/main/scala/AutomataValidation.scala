package automata

sealed trait RegistrationError

case object NameIsEmpty extends RegistrationError

case object StatesSetIsEmpty extends RegistrationError

case object AlphabetIsEmpty extends RegistrationError

case object StartStateIsEmpty extends RegistrationError
case object StartStateNotPartOfStatesSet extends RegistrationError

case object FinalStatesSetIsEmpty extends RegistrationError
case class FinalStatesNotSubsetOfAllStates(finalStates: Set[State]) extends RegistrationError

case object DeltaFunctionIsEmpty extends RegistrationError
case object DeltaFunctionIsIncompatible extends RegistrationError

object AutomataValidation {
  //!!! TO DO Check if automata with the same name is already saved in the file
  def validateName(name: String): Validated[RegistrationError, String] = {
    if (name.nonEmpty) Valid(name)
    else Invalid(Chain(NameIsEmpty))
  }

  def validateStatesSet(states: Set[State]): Validated[RegistrationError, Set[State]] = {
    if (states.nonEmpty) Valid(states)
    else Invalid(Chain(StatesSetIsEmpty))
  }

  def validateAlphabet(alphabet: Set[Letter]): Validated[RegistrationError, Set[Letter]] = {
    if (alphabet.nonEmpty) Valid(alphabet)
    else Invalid(Chain(AlphabetIsEmpty))
  }

  def validateStartState(startState: State, states: Set[State]): Validated[RegistrationError, State] = {
    val validateStartStateIsNotEmpty =
      if (startState.nonEmpty) Valid(startState)
      else Invalid(Chain(StartStateIsEmpty))

    val validateStartStateIsPartOfStates =
      if (states(startState)) Valid(startState)
      else Invalid(Chain(StartStateNotPartOfStatesSet))

    (
      validateStartStateIsNotEmpty,
      validateStartStateIsPartOfStates
    ).zip.map(_ => startState)
  }

  def validateFinalStatesSet(finalStates: Set[State], states: Set[State]): Validated[RegistrationError, Set[State]] = {
    val validateFinalStatesSetIsNotEmpty =
      if (finalStates.nonEmpty) Valid(finalStates)
      else Invalid(Chain(FinalStatesSetIsEmpty))

    val validateFinalStatesSetIsSubsetOfAllStates =
      if (finalStates.map(fs => fs.state).subsetOf(states.map(s => s.state))) Valid(finalStates)
      else Invalid(Chain(FinalStatesNotSubsetOfAllStates(finalStates)))

    (
      validateFinalStatesSetIsNotEmpty,
      validateFinalStatesSetIsSubsetOfAllStates
    ).zip.map(_ => finalStates)
  }

  def validateDeltaFunction(deltaFunction: types.DeltaFunction,
                            states: Set[State], alphabet: Set[Letter]): Validated[RegistrationError,types.DeltaFunction] = {
    val validateDeltaFunctionIsNotEmpty =
      if(deltaFunction.nonEmpty) Valid(deltaFunction)
      else Invalid(Chain(DeltaFunctionIsEmpty))

    def validateTransition(transition: ((State, Letter), State)): Boolean = transition match {
      case ((oldState, letter), newState) => states.map(s => s.state)(oldState.state) && alphabet.map(a => a.letter)(letter.letter) &&
        states.map(s => s.state)(newState.state)
    }

    val validateDeltaFunctionCompatibility =
      if (deltaFunction.forall(validateTransition)) Valid(deltaFunction)
      else Invalid(Chain(DeltaFunctionIsIncompatible))

    (
      validateDeltaFunctionIsNotEmpty,
      validateDeltaFunctionCompatibility
    ).zip.map(_ => deltaFunction)
  }
}