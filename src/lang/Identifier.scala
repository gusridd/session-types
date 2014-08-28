package lang

/**
 * Class for treating Strings like identifiers. This class 
 * gives them the ability to compare to each other and
 * to give a nice way to fully replace them.
 */
class Identifier(self: String) {
  def sub(target: String, replacement: String): String =
    if (self == target) replacement else self

  def minimum(other: String): String =
    if (self < other) self else other

  def maximum(other: String): String =
    if (self > other) self else other
}