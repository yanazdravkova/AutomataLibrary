import io.Console._
import io.IO

case class RegistrationForm(name: String,
                            states: Set[String], alphabet: Set[String],
                            startState: String, finalStates: Set[String],
                            deltaFunction: Map[(String, String), String])

object AutomataRegistration {
  def registerAutomata(form: RegistrationForm): Validated[RegistrationError, Automata] = {
    import AutomataValidation._
    (
      validateName(form.name),
      validateStatesSet(form.states),
      validateAlphabet(form.alphabet),
      validateStartState(form.startState, form.states),
      validateFinalStatesSet(form.finalStates, form.states),
      validateDeltaFunction(form.deltaFunction, form.states, form.alphabet),
    ).zipMap(Automata.apply)
  }
}


///probably move in separate file

object AutomataRegistrationApp {
  val registrationInput: IO[RegistrationForm] = for {
    name <- promptInput("ENTER AUTOMATA NAME")
    states <- statesInput("ENTER STATES")
    alphabet <- alphabetInput("ENTER ALPHABET")
    startState <- promptInput("ENTER START STATE")
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

  def ruleInput(prompt: String) : IO[((String, String), String)] = for {
    _ <- putStrLn(prompt)
    fromState <- promptInput("From state: ")
    withLetter <- promptInput("With letter: ")
    toState <- promptInput("To state: ")
  }yield  ((fromState, withLetter), toState)

  def statesInput(prompt: String, currStates: Set[String] = Set.empty): IO[Set[String]] = for {
    _ <- putStrLn(prompt)
    state <- promptInput("Enter state: ")
    updatedStates = currStates + state
    _ <- statesOutput(updatedStates)
    shouldContinue <- promptForContinuation
    _ <- if (shouldContinue) statesInput("Enter more states: ",updatedStates) else IO.unit
  } yield updatedStates

  def alphabetInput(prompt: String, currAlphabet: Set[String] = Set.empty): IO[Set[String]] = for {
    _ <- putStrLn(prompt)
    letter <- promptInput("Enter letter: ")
    updatedAlphabet = currAlphabet + letter
    _ <- alphabetOutput(updatedAlphabet)
    shouldContinue <- promptForContinuation
    _ <- if (shouldContinue) alphabetInput("Enter more letters: ", updatedAlphabet) else IO.unit
  } yield updatedAlphabet

  def deltaFunctionInput(prompt: String, currDeltaFuction: Map[(String, String), String] = Map.empty): IO[Map[(String, String), String]] = for {
    _ <- putStrLn(prompt)
    rule <- ruleInput("Enter rule: ")
    updatedDeltaFunction = currDeltaFuction + rule
    _ <- deltaOutput(updatedDeltaFunction)
    shouldContinue <- promptForContinuation
  } yield updatedDeltaFunction

  def statesOutput(set: Set[String]): IO[Unit] = for {
    _ <- putStrLn("Sates registered successfully:")
    _ <- putStrLn(set.toString)
  } yield ()

  def alphabetOutput(set: Set[String]): IO[Unit] = for {
    _ <- putStrLn("Alphabet registered successfully:")
    _ <- putStrLn(set.toString)
  } yield ()

  def deltaOutput(map:  Map[(String, String), String]): IO[Unit] = for {
    _ <- putStrLn("Delta function registered successfully:")
    _ <- putStrLn(map.toString)
  } yield ()

  def loop: IO[Unit] = for {
    registrationForm <- registrationInput
  //  shouldContinue <- promptForContinuation
   // _ <- if (shouldContinue) loop else IO.unit
  } yield ()

  def main(args: Array[String]): Unit = {
    AutomataRegistrationApp.loop.unsafeRun()
  }
}


