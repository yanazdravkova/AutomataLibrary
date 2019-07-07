package automata
import io.Console._
import io.IO

case class RegistrationForm(name: String,
                            states: Set[State], alphabet: Set[Letter],
                            startState: State, finalStates: Set[State],
                            deltaFunction: types.DeltaFunction)

object AutomataRegistration {
  def registerAutomata(form: RegistrationForm): Validated[RegistrationError, Automata] = {
    import AutomataValidation._
     val a = (
      validateName(form.name),
      validateStatesSet(form.states),
      validateAlphabet(form.alphabet),
      validateStartState(form.startState, form.states),
      validateFinalStatesSet(form.finalStates, form.states),
      validateDeltaFunction(form.deltaFunction, form.states, form.alphabet),
    ).zipMap(Automata.apply)
    a match {
      case Valid(a) => a.serialize()
    }
    return a
  }
}


///probably move in separate file

object AutomataRegistrationApp {
  val registrationInput: IO[RegistrationForm] = for {
    name <- promptInput("ENTER AUTOMATA NAME")
    states <- statesInput("ENTER STATES")
    alphabet <- alphabetInput("ENTER ALPHABET")
    startState <- startStateInput("ENTER START STATE")
    finalStates <- statesInput("ENTER FINAL STATES")
    deltaFunction <- deltaFunctionInput("ENTER DELTA FUNCTION")
  } yield RegistrationForm(name, states, alphabet, startState, finalStates, deltaFunction)

  def promptForContinuation: IO[Boolean] = for {
    input <- promptInput("Register more? (y/n)")
  } yield input == "y"

  def promptInput(prompt: String): IO[String] = for {
    _ <- putStrLn(prompt)
    input <- getStrLn
  } yield input

  def startStateInput(prompt: String): IO[State] = for {
    _ <- putStrLn(prompt)
    input <- getStrLn
  } yield State(input)

  def ruleInput(prompt: String) : IO[((State, Letter), State)] = for {
    _ <- putStrLn(prompt)
    fromState <- promptInput("From state: ")
    withLetter <- promptInput("With letter: ")
    toState <- promptInput("To state: ")
  }yield ((State(fromState), Letter(withLetter)), State(toState))

  def statesInput(prompt: String, currStates: Set[State] = Set.empty): IO[Set[State]] = for {
    _ <- putStrLn(prompt)
    state <- promptInput("Enter state: ")
    updatedStates = currStates + State(state)
    _ <- statesOutput(updatedStates)
    shouldContinue <- promptForContinuation
    _ <- if (shouldContinue) statesInput("Enter more states: ",updatedStates) else IO.unit
  } yield updatedStates

  def alphabetInput(prompt: String, currAlphabet: Set[Letter] = Set.empty): IO[Set[Letter]] = for {
    _ <- putStrLn(prompt)
    letter <- promptInput("Enter letter: ")
    updatedAlphabet = currAlphabet + Letter(letter)
    _ <- alphabetOutput(updatedAlphabet)
    shouldContinue <- promptForContinuation
    _ <- if (shouldContinue) alphabetInput("Enter more letters: ", updatedAlphabet) else IO.unit
  } yield updatedAlphabet

  def deltaFunctionInput(prompt: String, currDeltaFuction: types.DeltaFunction = Map.empty): IO[types.DeltaFunction] = for {
    _ <- putStrLn(prompt)
    rule <- ruleInput("Enter rule: ")
    updatedDeltaFunction = currDeltaFuction + rule
    _ <- deltaOutput(updatedDeltaFunction)
    shouldContinue <- promptForContinuation
    _ <- if(shouldContinue) deltaFunctionInput("Enter more rules: ", updatedDeltaFunction) else IO.unit
  } yield updatedDeltaFunction

  def statesOutput(set: Set[State]): IO[Unit] = for {
    _ <- putStrLn("Sates accepted successfully:")
    _ <- putStrLn(set.toString)
  } yield ()

  def alphabetOutput(set: Set[Letter]): IO[Unit] = for {
    _ <- putStrLn("Alphabet registered successfully:")
    _ <- putStrLn(set.toString)
  } yield ()

  def deltaOutput(map:  types.DeltaFunction): IO[Unit] = for {
    _ <- putStrLn("Delta function registered successfully:")
    _ <- putStrLn(map.toString)
  } yield ()

  def registrationOutput(automataValidation: Validated[RegistrationError, Automata]): IO[Unit] = automataValidation match {
    case Valid(automata) => for {
      _ <- putStrLn("Automata registered successfully:")
      _ <- putStrLn(automata.toString)
    } yield ()

    case Invalid(errors) =>
      errors
        .map(errorToDescription)
        .map(putStrLn)
        .foldLeft {
          putStrLn("The following errors have been found:")
        } {
          (acc, next) => acc.flatMap(_ => next)
        }
  }

  def errorToDescription(error: RegistrationError): String = error match{
    case NameIsEmpty => "Name is empty"

    case StatesSetIsEmpty => "States set is empty"

    case AlphabetIsEmpty => "Alphabet is empty"

    case StartStateIsEmpty => "Start state is empty"
    case StartStateNotPartOfStatesSet => "Start state is not part of states set"

    case FinalStatesSetIsEmpty => "Final set is empty"
    case FinalStatesNotSubsetOfAllStates(set) => "Final states set" + set.toString() + " is not subset of all states set"

    case DeltaFunctionIsEmpty => "Delta function is empty"
    case DeltaFunctionIsIncompatible => "Delta function is incompatible with the rest of the automata input"
  }

  def loop: IO[Unit] = for {
    registrationForm <- registrationInput
    validatedAutomata = AutomataRegistration.registerAutomata(registrationForm)
    _ <- registrationOutput(validatedAutomata)
  //  shouldContinue <- promptForContinuation
   // _ <- if (shouldContinue) loop else IO.unit
  } yield ()

  def main(args: Array[String]): Unit = {

    AutomataRegistrationApp.loop.unsafeRun()
  }
}


