import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

// The canonical machine: a binary incrementer.  Reading the tape from the
// right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
// stops; if the carry runs off the tape a new leading 1 is added.
def increment(line: String): String =
  val characters = ArrayBuffer.from(line)
  var index = characters.length - 1
  var carry = true
  while carry && index >= 0 do
    if characters(index) == '1' then
      characters(index) = '0'
      index -= 1
    else
      characters(index) = '1'
      carry = false
  (if carry then "1" else "") + characters.mkString

@main def turingMachine(): Unit =
  val raw = StdIn.readLine()
  var line = if raw == null then "" else raw
  if line.endsWith("\r") then line = line.dropRight(1)

  if line.exists(character => character != '0' && character != '1') then
    println("Invalid input: expected a binary string.")
  else
    println("Initial tape: " + line)
    println("Final tape: " + increment(line))
