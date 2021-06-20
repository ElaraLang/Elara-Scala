package io.github.elaralang.elara
package lexer

import scala.collection.mutable.ArrayBuffer

object Lexer {

  def lex(code: String): List[Token] = {

    val chars = code.toCharArray.toList
    val tokenReader = new TokenReader(chars, 0, new Position(0, 0))

    //val estimatedLength = estimateLength(code)
    //val tokens: Array[Token] = new Array[Token](estimatedLength)

    val tokens = new ArrayBuffer[Token]
    var i = 0

    var flag = false

    while (!flag) {

      val (tok, chars, line, col) = Scanner.read(tokenReader)

      if (tok == TokenType.EOF) {
        tokens.slice(i, tokens.length-1)
        flag = true
      }

      val token = new Token(tok, chars, new Position(line, col))

      if (i <= tokens.length-1) {
        tokens(i) = token
        i += 1
      } else {
        tokens.append(token)
        i = tokens.length
      }

    }

    tokens.toList

  }

  /*
  def estimateLength(code: String): Int = {
    var length = code.length
    if (length > 10) {
      length /= 2
    }
    length
  }*/

}
