package io.github.elaralang.elara
package lexer

object Chars {

  private val _eof = -1
  val eof: Char = _eof.toChar

  val illegalIdentifierChars = List(
    ',',
    '.',
    ':',
    '#',
    '[',
    ']',
    '(',
    '(',
    '{',
    '}',
    '"',
    '>',
    '<',
    ' ',
    '\n',
    '\r',
    '\t'
  )

  val whitespace = List(
    ' ',
    '\n',
    '\t'
  )

  val operatorSymbols = List(
    '=',
    '+',
    '-',
    '*',
    '/',
    '%',
    '&',
    '|',
    '^',
    '!',
    '>',
    '<'
  )

  def isWhitespace(char: Char): Boolean = {
    whitespace.contains(char.toString)
  }

  def isBracket(char: Char): Boolean = {
    char == '(' || char == ')' || char == '{' || char == '}' || char == '[' || char == ']' || isAngleBracket(char)
  }

  def isAngleBracket(char: Char): Boolean = {
    char == '<' || char == '>'
  }

  def isStartOfSymbol(char: Char): Boolean = {
    char == '.' || char == '='
  }

  def isValidIdentifier(char: Char): Boolean = {
    !illegalIdentifierChars.contains(char)
  }

  def isNumerical(char: Char): Boolean = {
    !char.isNaN
  }

  def isOperatorSymbol(char: Char): Boolean = {
    operatorSymbols.contains(char)
  }

  def charListEquals(a: List[Char], b: List[Char]): Boolean = {

    if (a.length != b.length) return false

    for (i <- a.indices) {
      if (a(i) != b(i)) return false
    }

    true

  }

}
