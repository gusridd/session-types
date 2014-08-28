package object lang {
  /**
   * Implicit conversion from String to Identifier. This makes
   * string replacement more easy.
   */
  implicit def stringToIdentifier(s: String) = new identifier(s)
}