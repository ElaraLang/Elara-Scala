package io.github.elaralang.elara
package lexer

class Position(var line: Int, var column: Int) {
  override def toString: String = {
    s"$line, $column"
  }
}