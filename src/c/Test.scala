package c

import java.io.FileReader

object Test {
  def main(args: Array[String]) {
    val context = new Context
    val reader = new FileReader("src/c/cddc.c")
    val scanner = new Scanner(reader)
    val parser = new Parser()

    scanner.context = context
    parser.context = context
    val res = parser.parse(scanner)
    println(res)
  }
}