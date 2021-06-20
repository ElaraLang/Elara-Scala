package io.github.elaralang.elara
package lexer

object TokenType extends Enumeration {
  type TokenType = Value

  // Special Tokens
  val ILLEGAL = Value("Illegal")
  val EOF = Value("EOF")
  val NEWLINE = Value("\\n")

  // Brackets
  val LParen = Value("LParen")
  val RParen = Value("RParen")
  val LBrace = Value("LBrace")
  val RBrace = Value("RBrace")
  val LAngle = Value("LAngle") // <
  val RAngle = Value("RAngle") // >
  val LSquare = Value("LSquare")
  val RSquare = Value("RSquare")

  // Keywords
  val Let = Value("Let")
  val Extend = Value("Extend")
  val Return = Value("Return")
  val While = Value("While")
  val Mut = Value("Mut")
  val Lazy = Value("Lazy")
  val Restricted = Value("Restricted")
  val Struct = Value("Struct")
  val Namespace = Value("Namespace")
  val Import = Value("Import")
  val Type = Value("Type")
  val If = Value("If")
  val Else = Value("Else")
  val Match = Value("Match")
  val As = Value("As")
  val Is = Value("Is")

  // Operators
  val Add = Value("Add")
  val Subtract = Value("Subtract")
  val Multiply = Value("Multiply")
  val Slash = Value("Slash")
  val Mod = Value("Mod")
  val And = Value("And")
  val Or = Value("Or")
  val Xor = Value("Xor")
  val Equals = Value("Equals")
  val NotEquals = Value("NotEquals")
  val GreaterEqual = Value("GreaterEqual")
  val LesserEqual = Value("LesserEqual")
  val Not = Value("Not")
  val TypeOr = Value("TypeOr") // |
  val TypeAnd = Value("TypeAnd") // &

  // Symbol
  val Equal = Value("Equal")
  val Arrow = Value("Arrow")
  val Dot = Value("Dot")

  // Literals
  val BooleanTrue = Value("True")
  val BooleanFalse = Value("False")
  val String = Value("String")
  val Char = Value("Char")
  val Int = Value("Int")
  val Float = Value("Float")
  val Comma = Value("Comma")
  val Colon = Value("Colon")
  val Identifier = Value("Identifier")
  val Underscore = Value("Underscore")

}
