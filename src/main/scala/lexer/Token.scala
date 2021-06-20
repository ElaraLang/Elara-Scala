package io.github.elaralang.elara
package lexer

import lexer.TokenType.TokenType

class Token(val tokenType: TokenType, val text: List[Char], val position: Position) {

  def equals(other: Token): Boolean = {
    if (other.tokenType == tokenType && Scanner.charListEquals(other.text, text) && other.position == position) return true
    false
  }

  override def toString: String = {
    s"${tokenType.toString} '${text.mkString("")}', ${position.toString}"
  }

}