package io.github.elaralang.elara
package lexer

import lexer.TokenType.{Colon, Comma, EOF, ILLEGAL, NEWLINE, TokenType}

object Scanner {

  def read(tokenReader: TokenReader): (TokenType, List[Char], Int, Int) = {

    val char = tokenReader.advance()

    char match {
      case Chars.eof => return (EOF, List(char), tokenReader.position.line, tokenReader.position.column)
      case '\r' => read(tokenReader)
      case '\n' =>
        val oldCol = tokenReader.position.column
        tokenReader.position.column = 0
        tokenReader.position.line += 1
        return (NEWLINE, List(char), tokenReader.position.line - 1, oldCol)
      case ',' =>
        // if this breaks, find DEFER FUNC equiv
        try {
          return (Comma, List(char), tokenReader.position.line, tokenReader.position.column)
        } finally {
          tokenReader.position.column += 1
        }

      case ':' =>
        // if this breaks, find DEFER FUNC equiv
        try {
          return (Colon, List(char), tokenReader.position.line, tokenReader.position.column)
        } finally {
          tokenReader.position.column += 1
        }
      case '"' =>
        val (str, t) = tokenReader.readString()
        try {
          return (str, t, tokenReader.position.line, tokenReader.position.column)
        } finally {
          tokenReader.position.column += t.length
        }
      case '\'' =>
        val (char, t) = tokenReader.readChar()
        try {
          return (char, List(t), tokenReader.position.line, tokenReader.position.column)
        } finally {
          tokenReader.position.column += 1
        }
    }

    if (Chars.isWhitespace(char)) {
      tokenReader.consumeWhitespace()
      return read(tokenReader)
    }
    if (Chars.isAngleBracket(char)) {
      tokenReader.unread()
      val (bracket, t) = tokenReader.readAngleBracket()
      try {
        return (bracket, t, tokenReader.position.line, tokenReader.position.column)
      } finally {
        tokenReader.position.column += t.length
      }
    }
    if (Chars.isStartOfSymbol(char)) {
      tokenReader.unread()
      val (symbol, t) = tokenReader.readSymbol()
      try {
        return (symbol, t, tokenReader.position.line, tokenReader.position.column)
      } finally {
        tokenReader.position.column += t.length
      }
    }
    if (Chars.isOperatorSymbol(char)) {
      tokenReader.unread()
      val (op, t) = tokenReader.readOperator()
      try {
        if (op != ILLEGAL && op != EOF) {
          return (op, t, tokenReader.position.line, tokenReader.position.column)
        }
      } finally {
        tokenReader.position.column += t.length
      }
    }
    if (Chars.isBracket(char)) {
      tokenReader.unread()
      val (number, t) = tokenReader.readNumber()
      try {
        return (number, t, tokenReader.position.line, tokenReader.position.column)
      } finally {
        tokenReader.position.column += t.length
      }
    }
    if (Chars.isNumerical(char)) {
      tokenReader.unread()
      val (number, t) = tokenReader.readNumber()
      try {
        return (number, t, tokenReader.position.line, tokenReader.position.column)
      } finally {
        tokenReader.position.column += t.length
      }
    }
    if (Chars.isValidIdentifier(char)) {
      tokenReader.unread()
      val (identifier, t) = tokenReader.readIdentifier()
      try {
        return (identifier, t, tokenReader.position.line, tokenReader.position.column)
      } finally {
        tokenReader.position.column += t.length
      }
    }

    (ILLEGAL, List(char), tokenReader.position.line, tokenReader.position.column)

  }

}
