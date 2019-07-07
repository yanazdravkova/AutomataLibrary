package automata

import io.Console._
import io.IO

object AutomataOperations {
  def traverseWord(automataName: String, word: List[Letter]): Checked[types.Path, State] = {
    val automata = Deserializer.deserialize(automataName)
    automata.traverseWord(word)
  }

  def toDotFormat(automataName: String): String = {
    val automata = Deserializer.deserialize(automataName)
    automata.toDotFormat()
  }
}

object AutomataOperationsApp {
  val traverseWordInput: IO[Checked[types.Path, State]] = for {
    automataName <- promptInput("ENTER AUTOMATA NAME IN WHICH TO TRAVERSE")
    word <- wordInput("ENTER WORD")
  } yield AutomataOperations.traverseWord(automataName, word)

  val toDotFormatInput: IO[String] = for {
    automataName <- promptInput("ENTER AUTOMATA NAME TO CONVERT IN DOT FORMAT")
  } yield AutomataOperations.toDotFormat(automataName)

  def promptInput(prompt: String): IO[String] = for {
    _ <- putStrLn(prompt)
    input <- getStrLn
  } yield input

  def wordInput(prompt: String, currWord: List[Letter] = List.empty): IO[List[Letter]] = for {
    letter <- promptInput("Next letter: ")
    updatedWord = currWord.appended(Letter(letter))
    _ <- wordOutput(updatedWord)
    shouldContinue <- promptForContinuation
    newUpdated <- if (shouldContinue) wordInput("Enter more letters: ", updatedWord) else IO(() => updatedWord)
  } yield newUpdated

  def promptForContinuation: IO[Boolean] = for {
    input <- promptInput("Register more? (y/n)")
  } yield input == "y"


  def wordOutput(word: List[Letter]): IO[Unit] = for {
    _ <- putStrLn("Alphabet registered successfully:")
    _ <- putStrLn(word.toString)
  } yield ()

  def traverseWordOutput(traverseWordFinished: Checked[types.Path, State]): IO[Unit] = traverseWordFinished match {
    case Success(path) => for {
      _ <- putStrLn("Word recognized from automata. The path was: ")
      _ <- putStrLn(path.toString())
    } yield ()

    case Failure(path, state) => for {
      _ <- putStrLn("Word not recognized. The path was: ")
      _ <- putStrLn(path.toString())
      _ <- putStrLn("Failed at state")
      _ <- putStrLn(state.toString)
    } yield ()
  }


  def toDotOutput(dot: String): IO[Unit] = for{
    _ <- putStrLn("IN DOT FORMAT")
    _ <- putStrLn(dot)
  } yield ()

  def traverseWord: IO[Unit] = for {
    traversedWord <- traverseWordInput
    _ <- traverseWordOutput(traversedWord)
  } yield ()

  def toDotFormat: IO[Unit] = for {
    converted <- toDotFormatInput
    _ <- toDotOutput(converted)
  } yield ()
}


