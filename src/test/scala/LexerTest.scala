package io.github.elaralang.elara

import lexer.{Lexer, Position, Token, TokenType}

import org.scalatest.FunSuite

class LexerTest extends FunSuite {

  test("TestIntAssignmnentLexing") {
    val code = "let a = 30"
    val tokens = Lexer.lex(code)

    val expected = List(
      new Token(TokenType.Let, "let".toCharArray.toList, new Position(0, 0)),
      new Token(TokenType.Identifier, List('a'), new Position(0, 4)),
      new Token(TokenType.Equal, List('='), new Position(0, 6)),
      new Token(TokenType.Int, "30".toCharArray.toList, new Position(0, 8))
    )

    assert(expected == tokens)

  }

}
