package lang

import lang.LocalChoice

import lang.wellformedness.ellFormedGlobalProtocol(exprs: List[expr]) extends GlobalProtocol(exprs) {
	Sanity(this)
	LocalChoice(this)

}