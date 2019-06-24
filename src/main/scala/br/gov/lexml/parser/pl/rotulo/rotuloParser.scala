package br.gov.lexml.parser.pl.rotulo

import scala.util.parsing.combinator._
import scala.language.postfixOps
import scala.util.parsing.input.CharArrayReader
import scala.util.matching._
import br.gov.lexml.parser.pl.text.normalizer

object rotuloParser {

	abstract sealed class Genero {
		def select[T](vMasc : T, vFem : T) : T
	}
	case object Masc extends Genero {
		def select[T](vMasc : T, vFem : T) = vMasc
	}
	case object Fem extends Genero {
		def select[T](vMasc : T, vFem : T) = vFem
	}

	import Character.{isLetter, isLetterOrDigit, isDigit}

	def mkString(cs : List[Any]) = cs.mkString("")

	def complementoToInteger (s : Seq[Char]) : Int = {
		s.foldLeft(0)({case  (n,c) => n * 26 + (c.toInt - ('a').toInt + 1)})-1
	}
	
	lazy val tipos = Seq(
		    "artigo","paragrafo","inciso","alinea",
		    "item", "pena", "parte", "livro",
		    "agregador","algumRotulo", "inteiro", 
		    "complemento", "ordinalExtenso", "numeroComposto", 
		    "simbOrdMasc", "simbOrdFem", "ordinalOuNatural", 
		    "ordinal", "numeroRomano")

	class RotuloParsers extends Parsers with RegexParsers with ImplicitConversions {
		lazy val eos : Parser[Unit] = Parser( (i : Input) => if (i.atEnd) {Success( (), i)} else {Failure("expected end of stream",i)})

		lazy val pos : Parser[Int] = Parser ( (i: Input) => Success(i.offset,i) )

		lazy val letter : Parser[Char] = elem("letter",isLetter)

		lazy val digit : Parser[Char] = elem("digit",isDigit)

		lazy val letterOrDigit : Parser[Char] = elem("letter or digit",isLetterOrDigit)

		lazy val hyphenCases = "‐‑‒–—―-－─━–−—" 
		lazy val hyphenOrSimilar : Parser[Elem] = 
          hyphenCases.to[Set].map(accept).reduceLeft( (x,y) => x | y)
		
		def romanOrString(s : String) : Either[String,Int] = {
			lazy val tryroman : Parser[Either[String,Int]] = (numeroRomano <~ eos) ^^ (Right(_))
			tryroman(new CharArrayReader(s.toCharArray())) match {
				case Success(n,_) => n
				case _ => Left(s)
			}

		}
		def atMost[R](p : Parser[R], n : Int) : Parser[List[R]] = n match {
			case 0 => success(List[R]())
			case _ => opt (p ~ atMost(p,n-1)) ^^ { case None => List[R]() ; case Some(~(c,cl)) => c :: cl }
		}
		def fromUpTo[R](p : Parser[R],from : Int, to : Int) : Parser[List[R]]= {
			repN(from,p) ~ atMost(p,to - from) ^^ (r => r._1 ++ r._2)
		}
		lazy val inteiro : Parser[Int] = ("\\d+"r) ^^ (Integer.parseInt(_))
		lazy val lowerAlpha : Parser[Char] = elem("Alpha",c => c >= 'a' && c <= 'z')
		lazy val complemento : Parser[Int] = hyphenOrSimilar ~> ( lowerAlpha + ) <~ guard(fimComplemento) ^^ complementoToInteger		

		lazy val fimComplemento : Parser[Unit] = (elem("fimComplemento", c => ",\\.;: ".contains(c)) ^^^ ()) | eos 
		def ordinalExtenso(g : Genero) : Parser[(Int,Boolean)] =
			( "unic" ^^^ (1,true)  | "primeir" ^^^ (1,false) | "segund" ^^^ (2,false) | "terceir" ^^^ (3,false)
			  | "quart" ^^^ (4,false) | "quint" ^^^ (5,false) | "sext" ^^^ (6,false) | "setim" ^^^ (7,false)
			  | "oitav" ^^^ (8,false) | "non" ^^^ (9,false) ) <~ g.select("o","a")
		lazy val numeroComposto : Parser[Int] =
			("\\d{1,3}+((\\.\\d\\d\\d)+|\\d*)"r) ^^ ((s : String) => { Integer.parseInt(s.toList.filter(c => c != '.').mkString("")) })
		def simbOrdinal(g : Genero) : Parser[Unit] = g.select("[oº°˚]"r,"[aª]"r) ~> not(letter | digit) ~> success()
		def ordinalOuNatural(g : Genero) : Parser[(Int,Boolean)] = ordinalExtenso(g) | ((numeroComposto <~ opt(simbOrdinal(g))) ^^ ((_ : Int,false)))
		def ordinal(g : Genero) : Parser[(Int,Boolean)] = ordinalExtenso(g) | ((numeroComposto <~ simbOrdinal(g))) ^^ ((_ : Int,false))

		lazy val numeroRomano : Parser[Int] = {
			lazy val milhares = rep('m') ^^ (_.length * 1000)
			lazy val centenas = pscheme('m','d','c') ^^ (_ * 100)
			lazy val dezenas  = pscheme('c','l','x') ^^ (_ * 10)
			lazy val unidades = pscheme('x','v','i')
			def pscheme(sX : Char,sV : Char,sI : Char) : Parser[Int] = {
				(sI ~> sX) ^^^ 9 |
				(sV ~> fromUpTo (sI,0,3) ^^ (_.length + 5)) |
				(sI ~> sV) ^^^ 4 |
				(fromUpTo (sI,1,3) ^^ (_.length)) |
				success(0)
			}
			(milhares ~ centenas ~ dezenas ~ unidades ^^ { case ~(~(~(m,c),d),u) => m+c+d+u })
				.^? ({ case n if n > 0 => n}, _ => "Números romanos não podem ser vazios.")
		}

		lazy val artigoUnico : Parser[RotuloArtigo] = "artigo unico." ^^^ RotuloArtigo(1,None,true)
		lazy val artigo : Parser[RotuloArtigo] =
			("art" ~> opt ("igo " | ".") ~> opt (" ")
			 ~> (ordinalOuNatural(Masc) ~ opt(complemento)) <~ opt(".")) ^^ ( p => RotuloArtigo(p._1._1,p._2,p._1._2))
		lazy val paragrafo1 : Parser[RotuloParagrafo] =
			((("""§( ["""  + hyphenCases + """]|\.)? ?""").r) ~> (ordinalOuNatural(Masc) ~ opt(complemento)) <~ opt(".")) ^^ {case ~((n,unico),c) => RotuloParagrafo(Some(n),c,unico) }
		lazy val paragrafo2 : Parser[RotuloParagrafo] = ("paragrafo " ~> ordinalExtenso(Masc)) ^^ {case (num,unico) => RotuloParagrafo(Some(num),None,unico)}
		lazy val paragrafoUnico : Parser[RotuloParagrafo] = "paragrafo unico." ^^^ RotuloParagrafo(Some(1),None,true)
		lazy val paragrafo : Parser[RotuloParagrafo] = paragrafoUnico | paragrafo2 | paragrafo1

		lazy val inciso : Parser[RotuloInciso] =
			(numeroRomano ~ opt(complemento)) <~ (' ' ?) <~ not (')') <~ (hyphenOrSimilar <~ rep(' ')) ^^ RotuloInciso

		lazy val alinea : Parser[RotuloAlinea] = {
			lazy val pnum : Parser[Int] = ("[a-z]+"r) ^^ (1 + complementoToInteger(_)) | ("\\d+"r) ^^ (Integer.parseInt(_))
			(pnum <~ opt (".")) ~ opt(complemento) <~ (" *\\)"r) ^^ RotuloAlinea
		}

		lazy val item : Parser[RotuloItem] = ( 		    
		    (inteiro ~ opt(complemento)) <~ opt(elem(' ')) <~ (hyphenOrSimilar | opt(elem('.'))) <~ opt(elem(' ')) 
		    ) ^^ RotuloItem

		lazy val pena : Parser[Rotulo] = "pena -" ^^^ RotuloPena

		lazy val parte : Parser[RotuloParte] = {
			lazy val rotTexto : Parser[String] = ("parte " ~> ("\\w+"r) ) | ("p a r t e" ~> rep(' ' ~> letterOrDigit) ^^ mkString)

			rotTexto ~ opt(complemento) ^^ {case ~(num,cmp) => RotuloParte(romanOrString(num),cmp)}
		}

		lazy val livro : Parser[RotuloLivro] = {
			("livro " ~> ("\\w+"r)) ~ opt(complemento) ^^ {case ~(num,cmp) => RotuloLivro(romanOrString(num),cmp)}
		}

		lazy val agregador : Parser[Rotulo] = {
			lazy val tipo : Parser[(Int,Option[Int]) => Rotulo] = (
				  "livro" ^^^ ((n : Int, c : Option[Int]) => RotuloLivro(Right(n),c))
				| "titulo" ^^^ RotuloTitulo
				| "subtitulo" ^^^ RotuloSubTitulo
				| "capitulo" ^^^ RotuloCapitulo
				| "subcapitulo" ^^^ RotuloCapitulo
				| "secao" ^^^ RotuloSecao
				| "subsecao" ^^^ RotuloSubSecao
			)
			(tipo <~ ' ') ~ (("unico" ^^^ 1) | ("unica" ^^^ 1) | numeroRomano) ~ opt(complemento) ^^ { case ~(~(t,n),c) => t (n,c) }
		}

		lazy val algumRotulo : Parser[Rotulo] = ( artigo | paragrafo | inciso | alinea | item | pena | parte | livro | agregador ) <~ " *-? *".r

		def parserPorTipo(tipo : String) : Parser[Any] = tipo match {
			case "artigo" => artigo
			case "paragrafo" => paragrafo
			case "inciso" => inciso
			case "alinea" => alinea
			case "item" => item
			case "pena" => pena
			case "parte" => parte
			case "livro" => livro			
			case "agregador" => agregador
			case "algumRotulo" => algumRotulo
			case "inteiro" => inteiro
			case "complemento" => complemento
			case "ordinalExtenso" => ordinalExtenso (Masc)
			case "numeroComposto" => numeroComposto
			case "simbOrdMasc" => simbOrdinal (Masc)
			case "simbOrdFem" => simbOrdinal (Fem)
			case "ordinalOuNatural" => ordinalOuNatural (Masc)
			case "ordinal" => ordinal (Masc)
			case "numeroRomano" => numeroRomano			
		}							

		lazy val pontuacao  = (" *\\."r) ~> not (".") ^^^ ()

		def parseRotulo(s : String) : Option[(Rotulo,Int)] = {
		    val p = (algumRotulo <~ opt (pontuacao)) ~ pos ^^ { case ~(a,b) => (a,b) }
		    p(new CharArrayReader(s.toCharArray())) match {
		    	case Success(r,_) => Some(r)
		    	case _ => None
		    }
		}
		
		def testComplemento(s : String) = {
		    val p = complemento
		    p(new CharArrayReader(s.toCharArray())) match {
		    	case Success(r,_) => Some(r)
		    	case _ => None
		    }
		}
		override def skipWhitespace = false
	}

	val parseRotulo = (new RotuloParsers).parseRotulo(_)
	
	def test(s : String) : Option[Int] = (new RotuloParsers).testComplemento(s)

	def main(args : Array[String]) {
	  println("Testing rotulos:")
	  val (tps,inputs1) = if(args.length == 0) {
	    val lines = {
	      val br = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
	      val b = Seq.newBuilder[String]
	      var l = ""
	      while( { l = br.readLine() ; l != null && l != "" }) {
	        b += l
	      }
	      b.result()
	    }
	    (tipos, lines)			
		} else if (args.length == 1) {
		  (tipos, Seq(args(0)))
		} else {
			(Seq(args(0)),args.slice(1,args.length).to[Seq])
		}
	  val inputs = inputs1.map(normalizer.normalize)
	  println(s"tps = ${tps}, inputs={$inputs}")
	  inputs.foreach { x =>
	    println("Input: " + x)
	    tipos.foreach { t =>
	      println(s"  [${t}]: ${test(t,x).toString.replaceAll("[\\n\\r]"," - ")}")
	    }
	    println()
	  }	  
	}

	def test(tipo : String, text : String) = {
		    val ps = new RotuloParsers
		    lazy val p = (ps.parserPorTipo(tipo) ~ (ps.pos ^^ ((n : Int) => text.substring(n))))
			p(new CharArrayReader(text.toCharArray()))
	}

}
