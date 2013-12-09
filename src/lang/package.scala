package object lang {
	implicit def stringToIdentifier(s: String) = new identifier(s)
}