package br.gov.lexml.parser.pl.text

object normalizer:
  def normalize(s: String): String =
    import java.text.Normalizer
    val s1 = Normalizer
      .normalize(s, Normalizer.Form.NFD)
      .replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    removeDuplicateSpace(s1.toLowerCase.map(cleanchar).trim)
  
  private def removeDuplicateSpace(s: String) =
    s.replaceAll("""\p{javaWhitespace}+""", " ")

  private val cleanMap : Array[Char] =
    val m = Map(
        'á' -> 'a',
        'à' -> 'a',
        'ã' -> 'a',
        'â' -> 'a',
        'é' -> 'e',
        'ê' -> 'e',
        'í' -> 'i',
        'ó' -> 'o',
        'ô' -> 'o',
        'õ' -> 'o',
        'ú' -> 'u',
        'û' -> 'u',
        'ü' -> 'u',
        'ç' -> 'c'
        )  ++ "‐‑‒–—―-﹢－─━-–−—".map(c => c -> '-')
    Array.tabulate(m.keys.max + 1)(c => m.getOrElse(c.toChar,c.toChar))

  @inline private def cleanchar(c: Char): Char = if c < cleanMap.length then cleanMap(c.toInt) else c