package object lang {
	implicit def stringToReplaceable(s: String) = new identifier(s)
}