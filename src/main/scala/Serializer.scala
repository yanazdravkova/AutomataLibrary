package  automata

import java.io.{BufferedWriter, File, FileWriter}

import io.Console.putStrLn
import io.IO

object constantFileNames {
  val fileWithAutomataNames = "D:/REPOS/Automatons/fileNames.txt"
}
//fileNames - file where the names of  all the created automatas already are saved
case class Serializer() {
  def formatRules(deltaFunction: types.DeltaFunction): String = deltaFunction.foldLeft(""){ (s: String, pair: ((State, Letter), State)) =>
    s + pair._1._1.state + " " + pair._1._2.letter + " " + pair._2.state + "\r\n"}

  /*
  Serialization format:
  n //number of states
  m //number of letters
  k // number of final states
  l // number of rules in delta function
  lines of rule in the format: oldState letter newState

  ex:
  4
  3
  2
  3
  1 a 2
  2 b 3
  1 c 4
   */
  def formatAutomataForFile(automata: Automata): String = { automata.name + "\r\n" +
                                                            automata.states.size.toString + "\r\n" +
                                                            automata.alphabet.size.toString + "\r\n" +
                                                            automata.finalStates.size.toString + "\r\n" +
                                                            automata.deltaFunction.size.toString + "\r\n" +
                                                            formatRules(automata.deltaFunction)}

  def print(automata: Automata): IO[Unit] = for {
    _ <- putStrLn("formated Automata")
    formated = formatAutomataForFile(automata)
    _ <- putStrLn(formated)
  } yield ()

  def saveNameInFileWithAutomataNames(name: String) = {
    val file = new File(constantFileNames.fileWithAutomataNames)
    val buffWriter = new BufferedWriter(new FileWriter(file, true))
    buffWriter.write(name + "\r\n")
    buffWriter.close()
  }

  def serialize(automata: Automata): Unit ={
    saveNameInFileWithAutomataNames(automata.name)

    val formated: String = formatAutomataForFile(automata)
    val file = new File(automata.name + ".txt")
    file.createNewFile()

    val buffWriter = new BufferedWriter(new FileWriter(file))
    buffWriter.write(formated)
    buffWriter.close()
  }
}
