package br.gov.lexml.parser.pl.text

object normalizer {
	def normalize(s : String) : String = removeDuplicateSpace(s.toLowerCase.map(cleanchar).trim)
	def removeDuplicateSpace(s : String) = s.replaceAll("""\p{javaWhitespace}+"""," ")
	def cleanchar(c : Char) : Char = c match {
		case 'á' => 'a'
		case 'à' => 'a'
		case 'ã' => 'a'
		case 'â' => 'a'
		case 'é' => 'e'
		case 'ê' => 'e'
		case 'í' => 'i'		
		case 'ó' => 'o'
		case 'ô' => 'o'
		case 'õ' => 'o'
		case 'ú' => 'u'
		case 'û' => 'u'
		case 'ü' => 'u'
		case 'ç' => 'c'
		case '–' => '-'
		case '−' => '-'
		case '—' => '-'
		case _ => c
	}
}