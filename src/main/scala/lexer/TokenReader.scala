package io.github.elaralang.elara
package lexer

import lexer.TokenType.{Add, And, Arrow, As, BooleanFalse, BooleanTrue, Dot, Else, Equal, Equals, Extend, GreaterEqual, ILLEGAL, Identifier, If, Import, Is, LAngle, LBrace, LParen, LSquare, Lazy, LesserEqual, Let, Match, Mod, Multiply, Mut, Namespace, Not, NotEquals, Or, RAngle, RBrace, RParen, RSquare, Restricted, Return, Slash, Struct, Subtract, TokenType, Type, TypeOr, While, Xor}

class TokenReader(val chars: List[Char], var cursor: Int, var position: Position) {

  def advance(): Char = {
    if (cursor >= chars.length) {
      return Chars.eof
    }

    val char = chars(cursor)
    cursor += 1

    char

  }

  def unread(): Unit = cursor -= 1

  def peek(): Char = {
    if (cursor >= chars.length-1) {
      return Chars.eof
    }
    chars(cursor)
  }

  def consumeWhitespace(): Int = {
    var count = 1

    var exit = false
    while (!exit) {
      val char = advance()
      char match {
        case Chars.eof => exit = true
        case '\n' =>
          position.line += 1
          position.column = 0
        case '\t' => count += 1
        case ' ' => count += 1
        case _ =>
          unread()
          exit = true
      }
    }

    position.column += count
    count

  }

  def readBracket(): (TokenType, List[Char]) = {

    val str = advance()
    val strList = List(str)
    str match {
      case '(' => (LParen, strList)
      case ')' => (RParen, strList)
      case '{' => (LBrace, strList)
      case '}' => (RBrace, strList)
      case '<' => (LAngle, strList)
      case '>' => (RAngle, strList)
      case '[' => (LSquare, strList)
      case ']' => (RSquare, strList)
      case _ => (ILLEGAL, strList)
    }

  }

  def readSymbol(): (TokenType, List[Char]) = {

    val char = advance()
    val charList = List(char)

    char match {
      case '.' => (Dot, charList)
      case '=' =>
        val peeked = peek()
        peeked match {
          case '>' =>
            advance()
            (Arrow, charList)
          case '=' =>
            advance()
            (Equals, charList)
          case _ => (Equal, charList)
        }
      case _ => (ILLEGAL, charList)
    }

  }

  def readAngleBracket(): (TokenType, List[Char]) = {

    val next = advance()
    val char = peek()
    val charList = List(next, char)

    next match {
      case '<' =>
        char match {
          case '=' =>
            advance()
            (LesserEqual, charList)
          case _ => (LAngle, charList)
        }
      case '>' =>
        char match {
          case '=' =>
            advance()
            (GreaterEqual, charList)
          case _ => (RAngle, charList)
        }
      case _ => (ILLEGAL, charList)
    }

  }

  def readString(): (TokenType, List[Char]) = {

    val start = cursor
    var end = start
    var flag = false

    while (!flag) {
      val char = chars(end)

      if (char == Chars.eof) {
        flag = true
      }

      end += 1

      if (char == '"') {
        flag = true
      }

      if (end >= chars.length) {
        flag = true
      }

    }

    cursor = end
    (TokenType.String, chars.slice(start, end-1))

  }

  def readChar(): (TokenType, Char) = {

    val start = cursor
    var char = chars(start)
    cursor += 1

    if (chars(cursor-1) == '\\') {
      chars(cursor) match {
        case 'n' => char = '\n'
        case 'r' => char = '\r'
        case 't' => char = '\t'
        case '\'' => char = '\''
        case '\\' => char = '\\'
        case 'b' => char = '\b'
        case _ => throw new IllegalArgumentException("Char literal must only have 1 symbol")
      }
      cursor += 1
    }

    if (chars(cursor) == '\'') {
      throw new IllegalArgumentException("Char literal must only have 1 symbol")
    }

    cursor += 1
    (TokenType.Char, char)

  }

  def readNumber(): (TokenType, List[Char]) = {

    val start = cursor
    var end = start
    var numType = TokenType.Int
    var flag = false

    while (!flag) {
      val r = chars(end)

      r match {
        case Chars.eof => flag = true
        case '\n' =>
          unread()
          flag = true
        case '.' => {
          if (numType == TokenType.Float) {
            flag = true
          }
          numType = TokenType.Float
        }
      }

      if (r.isNaN) {
        unread()
        flag = true
      }

      end += 1

      if (end >= chars.length) {
        flag = true
      }

    }

    cursor = end
    (numType, chars.slice(start, end))

  }

  private def checkUnknownOperator(chars: List[Char]) = {
    val n = chars(1)
    if (chars.length > 2 || n != '=') {
      unkownOperatorError()
    }
  }

  private def unkownOperatorError() = {
    throw new IllegalArgumentException("Unknown operator $str")
  }

  def readOperator(): (TokenType, List[Char]) = {

    val start = cursor
    var end = start
    var flag = false

    while (!flag) {
      val char = chars(end)

      if (char == Chars.eof) {
        flag = true
      }
      if (!Chars.isOperatorSymbol(char)) {
        unread()
        flag = true
      }
      end += 1

      if (end >= chars.length) {
        flag = true
      }

    }

    cursor = end

    val str = chars.slice(start, end)
    str match {
      case '+' => return (Add, str)
      case '-' => return (Subtract, str)
      case '*' => return (Multiply, str)
      case '/' => return (Slash, str)
      case '%' => return (Mod, str)
      case '^' => return (Xor, str)
      case '|' => return (TypeOr, str)
      case '>' =>
        if (str.length == 1) {
          return (LAngle, str)
        }

        checkUnknownOperator(str)

        return (GreaterEqual, str)
      case '<' =>
        if (str.length == 1) {
          return (RAngle, str)
        }

        checkUnknownOperator(str)

        return (LesserEqual, str)
      case '!' =>
        if (str.length == 1) {
          return (Not, str)
        }

        checkUnknownOperator(str)

        return (NotEquals, str)

    }

    if (str.length != 2) {
      unkownOperatorError()
    }

    if (Chars.charListEquals(str, List('&', '&'))) {
      return (And, str)
    }
    if (Chars.charListEquals(str, List('|', '|'))) {
      return (Or, str)
    }
    if (Chars.charListEquals(str, List('=', '='))) {
      return (Equals, str)
    }

    (ILLEGAL, str)

  }

  def readIdentifier(): (TokenType, List[Char]) = {

    val start = cursor
    var end = start
    var flag = false

    while (!flag) {

      val char = chars(end)

      if (char == Chars.eof || !Chars.isValidIdentifier(char)) {
        flag = true
      }

      end += 1

      if (end >= chars.length) {
        flag = true
      }

    }

    cursor = end

    val str = chars.slice(start, end)
    val length = end - start

    str.head match {
      case 'l' =>
        if (length == 3 && str(1) == 'e' && str(2) == 't') {
          return (Let, str)
        }
        if (length == 4 && str(1) == 'a' && str(2) == 'z' && str(3) == 'y') {
          return (Lazy, str)
        }
        return (Identifier, str)
      case 't' =>
        if (length == 4) {
          if (str(1) == 'y' && str(2) == 'p' && str(3) == 'e') {
            return (Type, str)
          }
          if (str(1) == 'r' && str(2) == 'u' && str(3) == 'e') {
            return (BooleanTrue, str)
          }
          return (Identifier, str)
        }
      case 'i' =>
        if (length == 2) {
          if (str(1) == 'f') {
            return (If, str)
          }
          if (str(1) == 's') {
            return (Is, str)
          }
        }
        if (length == 6 && str(1) == 'm' && str(2) == 'p' && str(3) == 'o' && str(4) == 'r' && str(5) == 't') {
          return (Import, str)
        }
        return (Identifier, str)
    }

    if (Chars.charListEquals(str, List('m', 'u', 't'))) {
      return (Mut, str)
    }
    if (Chars.charListEquals(str, List('r', 'e', 's', 't', 'r', 'i', 'c', 't', 'e', 'd'))) {
      return (Restricted, str)
    }
    if (Chars.charListEquals(str, List('e', 'x', 't', 'e', 'n', 'd'))) {
      return (Extend, str)
    }
    if (Chars.charListEquals(str, List('r', 'e', 't', 'u', 'r', 'n'))) {
      return (Return, str)
    }
    if (Chars.charListEquals(str, List('w', 'h', 'i', 'l', 'e'))) {
      return (While, str)
    }
    if (Chars.charListEquals(str, List('s', 't', 'r', 'u', 'c', 't'))) {
      return (Struct, str)
    }
    if (Chars.charListEquals(str, List('n', 'a', 'm', 'e', 's', 'p', 'a', 'c', 'e'))) {
      return (Namespace, str)
    }
    if (Chars.charListEquals(str, List('e', 'l', 's', 'e'))) {
      return (Else, str)
    }
    if (Chars.charListEquals(str, List('m', 'a', 't', 'c', 'h'))) {
      return (Match, str)
    }
    if (Chars.charListEquals(str, List('a', 's'))) {
      return (As, str)
    }
    if (Chars.charListEquals(str, List('f', 'a', 'l', 's', 'e'))) {
      return (BooleanFalse, str)
    }

    (Identifier, str)

  }

}
