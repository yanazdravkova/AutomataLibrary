package automata

import java.io.{BufferedWriter, File, FileWriter}

import io.Console.putStrLn
import io.IO

import scala.io.Source

object constantFileNames {
  val fileWithAutomataNames = "D:/REPOS/Automatons/fileNames.txt"
}

//fileNames - file where the names of  all the created automatas already are saved
object Serializer {
  /*
  Serialization format:
  states
  letters
  start state
  final states
  l // number of rules in delta function
  lines of rule in the format: oldState letter newState

  ex:
  test
  1 2 3 4
  a b c
  1
  3 4
  2
  1 a 2
  2 b 3
   */
  def formatAutomataForFile(automata: Automata): String = {
    automata.name + "\r\n" +
      formatStates(automata.states) +
      formatAlphabet(automata.alphabet) +
      automata.startState.state + "\r\n" +
      formatStates(automata.finalStates) +
      automata.deltaFunction.size.toString + "\r\n" +
      formatRules(automata.deltaFunction)
  }

  def formatStates(states: Set[State]): String = (for (s <- states) yield s.state).mkString(" ") + "\r\n"

  def formatAlphabet(alphabet: Set[Letter]): String = (for (l <- alphabet) yield l.letter).mkString(" ") + "\r\n"

  def formatRules(deltaFunction: types.DeltaFunction): String = deltaFunction.foldLeft("") { (s: String, pair: ((State, Letter), State)) =>
    s + pair._1._1.state + " " + pair._1._2.letter + " " + pair._2.state + "\r\n"
  }

  def print(automata: Automata): IO[Unit] = for {
    _ <- putStrLn("formated Automata")
    formated = formatAutomataForFile(automata)
    _ <- putStrLn(formated)
  } yield ()

  def convertToFileName(name: String): String = name + ".txt"

  def saveNameInFileWithAutomataNames(name: String) = {
    val file = new File(constantFileNames.fileWithAutomataNames)
    val buffWriter = new BufferedWriter(new FileWriter(file, true))
    buffWriter.write(name + "\r\n")
    buffWriter.close()
  }

  def serialize(automata: Automata): Unit = {
    saveNameInFileWithAutomataNames(automata.name)

    val formated: String = formatAutomataForFile(automata)
    val file = new File(convertToFileName(automata.name))
    file.createNewFile()

    val buffWriter = new BufferedWriter(new FileWriter(file))
    buffWriter.write(formated)
    buffWriter.close()
  }
}
