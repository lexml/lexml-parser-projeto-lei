package br.gov.lexml.parser.pl.util

object Entities {

  val entities = Map[String,Char](
    "aacute " -> 'á',
    "Aacute " -> 'Á',
    "abreve " -> 'ă',
    "Abreve " -> 'Ă',
    "ac" -> '∾',
    "acd " -> '∿',
    "acirc " -> 'â',
    "Acirc " -> 'Â',
    "acute" -> '´',
    "acy " -> 'а',
    "Acy " -> 'А',
    "add " -> '+',
    "aelig " -> 'æ',
    "AElig " -> 'Æ',
    "agrave " -> 'à',
    "Agrave " -> 'À',
    "alefsym" -> 'ℵ',
    "aleph " -> 'ℵ',
    "alpha " -> 'α',
    "Alpha " -> 'Α',
    "amacr " -> 'ā',
    "Amacr " -> 'Ā',
    "amalg " -> '⨿',
    "amp" -> '&',
    "AMP " -> '&',
    "and" -> '∧',
    "And " -> '⩓',
    "andand " -> '⩕',
    "andd " -> '⩜',
    "andslope " -> '⩘',
    "andv " -> '⩚',
    "ang" -> '∠',
    "ange " -> '⦤',
    "angle " -> '∠',
    "angmsd" -> '∡',
    "angmsdaa " -> '⦨',
    "angmsdab " -> '⦩',
    "angmsdac " -> '⦪',
    "angmsdad " -> '⦫',
    "angmsdae " -> '⦬',
    "angmsdaf " -> '⦭',
    "angmsdag " -> '⦮',
    "angmsdah " -> '⦯',
    "angrt " -> '∟',
    "angrtvb " -> '⊾',
    "angrtvbd " -> '⦝',
    "angsph " -> '∡',
    "angst " -> 'Å',
    "angzarr " -> '⍼',
    "aogon " -> 'ą',
    "Aogon " -> 'Ą',
    "ap" -> '≈',
    "ape" -> '≊',
    "apid " -> '≋',
    "apos " -> ''',
    "apos" -> '\'',
    "approx" -> '≈',
    "approxeq " -> '≊',
    "aring " -> 'å',
    "Aring " -> 'Å',
    "Assign " -> '≔',
    "ast" -> '*',
    "asymp" -> '≈',
    "asympeq" -> '≍',
    "atilde " -> 'ã',
    "Atilde " -> 'Ã',
    "auml " -> 'ä',
    "Auml " -> 'Ä',
    "awconint" -> '∲',
    "awint " -> '⨑',
    "backcong " -> '≌',
    "backepsilon " -> '϶',
    "backprime " -> '‵',
    "backsim " -> '∽',
    "backsimeq " -> '⋍',
    "Backslash" -> '∖',
    "barvee " -> '⊽',
    "barwed" -> '⌅',
    "Barwed" -> '⌆',
    "barwedge " -> '⌅',
    "bbrk" -> '⎵',
    "bbrktbrk " -> '⎶',
    "bcong" -> '≌',
    "bcy " -> 'б',
    "Bcy " -> 'Б',
    "bdquo" -> '„',
    "becaus" -> '∵',
    "because" -> '∵',
    "Because " -> '∵',
    "bemptyv " -> '⦰',
    "bepsi" -> '϶',
    "bernou" -> 'ℬ',
    "Bernoullis" -> 'ℬ',
    "beta " -> 'β',
    "Beta " -> 'Β',
    "beth " -> 'ℶ',
    "between " -> '≬',
    "bigcap " -> '⋂',
    "bigcirc " -> '◯',
    "bigcup " -> '⋃',
    "bigodot " -> '⨀',
    "bigoplus " -> '⨁',
    "bigotimes " -> '⨂',
    "bigsqcup " -> '⨆',
    "bigstar " -> '★',
    "bigtriangledown " -> '▽',
    "bigtriangleup " -> '△',
    "biguplus " -> '⨄',
    "bigvee " -> '⋁',
    "bigwedge " -> '⋀',
    "bkarow " -> '⤍',
    "blacklozenge " -> '⧫',
    "blacksquare" -> '▪',
    "blacktriangle " -> '▴',
    "blacktriangledown " -> '▾',
    "blacktriangleleft " -> '◂',
    "blacktriangleright " -> '▸',
    "blank " -> '␣',
    "blk12 " -> '▒',
    "blk14 " -> '░',
    "blk34 " -> '▓',
    "block " -> '█',
    "bnot " -> '⌐',
    "bot" -> '⊥',
    "bottom" -> '⊥',
    "bowtie " -> '⋈',
    "boxbox " -> '⧉',
    "boxdl " -> '┐',
    "boxdL " -> '╕',
    "boxDl " -> '╖',
    "boxDL " -> '╗',
    "boxdr " -> '┌',
    "boxdR " -> '╒',
    "boxDr " -> '╓',
    "boxDR " -> '╔',
    "boxh" -> '─',
    "boxH " -> '═',
    "boxhd " -> '┬',
    "boxhD " -> '╥',
    "boxHd " -> '╤',
    "boxHD " -> '╦',
    "boxhu " -> '┴',
    "boxhU " -> '╨',
    "boxHu " -> '╧',
    "boxHU " -> '╩',
    "boxminus " -> '⊟',
    "boxplus " -> '⊞',
    "boxtimes " -> '⊠',
    "boxul " -> '┘',
    "boxuL " -> '╛',
    "boxUl " -> '╜',
    "boxUL " -> '╝',
    "boxur " -> '└',
    "boxuR " -> '╘',
    "boxUr " -> '╙',
    "boxUR " -> '╚',
    "boxv " -> '│',
    "boxV " -> '║',
    "boxvh " -> '┼',
    "boxvH " -> '╪',
    "boxVh " -> '╫',
    "boxVH " -> '╬',
    "boxvl " -> '┤',
    "boxvL " -> '╡',
    "boxVl " -> '╢',
    "boxVL " -> '╣',
    "boxvr " -> '├',
    "boxvR " -> '╞',
    "boxVr " -> '╟',
    "boxVR " -> '╠',
    "bprime" -> '‵',
    "breve" -> '˘',
    "Breve " -> '˘',
    "brvbar " -> '¦',
    "Bscr " -> 'ℬ',
    "bsemi " -> '⁏',
    "bsim" -> '∽',
    "bsime" -> '⋍',
    "bsol " -> '\\',
    "bsolb " -> '⧅',
    "bull" -> '•',
    "bullet " -> '•',
    "bump" -> '≎',
    "bumpe" -> '≏',
    "Bumpeq " -> '≎',
    "cacute " -> 'ć',
    "Cacute " -> 'Ć',
    "cap " -> '∩',
    "Cap " -> '⋒',
    "capand " -> '⩄',
    "capbrcup " -> '⩉',
    "capcap " -> '⩋',
    "capcup " -> '⩇',
    "capdot " -> '⩀',
    "CapitalDifferentialD" -> 'ⅅ',
    "caret " -> '⁁',
    "caron" -> 'ˇ',
    "Cayleys " -> 'ℭ',
    "ccaps " -> '⩍',
    "ccaron " -> 'č',
    "Ccaron " -> 'Č',
    "ccedil " -> 'ç',
    "Ccedil " -> 'Ç',
    "ccirc " -> 'ĉ',
    "Ccirc " -> 'Ĉ',
    "Cconint " -> '∰',
    "ccups " -> '⩌',
    "cdot " -> 'ċ',
    "Cdot " -> 'Ċ',
    "cedil" -> '¸',
    "Cedilla " -> '¸',
    "cemptyv " -> '⦲',
    "cent " -> '¢',
    "centerdot" -> '·',
    "CenterDot " -> '·',
    "Cfr" -> 'ℭ',
    "chcy " -> 'ч',
    "CHcy " -> 'Ч',
    "check" -> '✓',
    "checkmark " -> '✓',
    "chi " -> 'χ',
    "Chi " -> 'Χ',
    "cir " -> '○',
    "circ " -> 'ˆ',
    "circeq " -> '≗',
    "circlearrowleft " -> '↺',
    "circlearrowright " -> '↻',
    "circledast " -> '⊛',
    "circledcirc " -> '⊚',
    "circleddash " -> '⊝',
    "CircleDot " -> '⊙',
    "circledR" -> '®',
    "circledS " -> 'Ⓢ',
    "CircleMinus " -> '⊖',
    "CirclePlus " -> '⊕',
    "CircleTimes " -> '⊗',
    "cire" -> '≗',
    "cirE " -> '⧃',
    "cirfnint " -> '⨐',
    "cirscir " -> '⧂',
    "ClockwiseContourIntegral " -> '∲',
    "CloseCurlyDoubleQuote " -> '”',
    "CloseCurlyQuote " -> '’',
    "clubs" -> '♣',
    "clubsuit " -> '♣',
    "colon " -> ':',
    "Colon" -> '∷',
    "colone" -> '≔',
    "coloneq" -> '≔',
    "comma " -> ',',
    "commat " -> '@',
    "comp" -> '∁',
    "compfn" -> '∘',
    "complement " -> '∁',
    "complexes " -> 'ℂ',
    "cong" -> '≅',
    "Congruent " -> '≡',
    "conint" -> '∮',
    "Conint" -> '∯',
    "ContourIntegral " -> '∮',
    "Copf" -> 'ℂ',
    "coprod" -> '∐',
    "Coproduct " -> '∐',
    "copy" -> '©',
    "COPY " -> '©',
    "copysr " -> '℗',
    "CounterClockwiseContourIntegral " -> '∲',
    "crarr " -> '↵',
    "cross " -> '✗',
    "Cross " -> '⨯',
    "ctdot " -> '⋯',
    "cudarrl " -> '⤸',
    "cudarrr " -> '⤵',
    "cuepr" -> '⋞',
    "cuesc" -> '⋟',
    "cularr" -> '↶',
    "cularrp " -> '⤽',
    "cup " -> '∪',
    "Cup " -> '⋓',
    "cupbrcap " -> '⩈',
    "cupcap " -> '⩆',
    "CupCap " -> '≍',
    "cupcup " -> '⩊',
    "cupdot " -> '⊍',
    "cupor " -> '⩅',
    "curarr" -> '↷',
    "curarrm " -> '⤼',
    "curlyeqprec " -> '⋞',
    "curlyeqsucc " -> '⋟',
    "curlyvee " -> '⋎',
    "curlywedge " -> '⋏',
    "curren " -> '¤',
    "curvearrowleft " -> '↶',
    "curvearrowright " -> '↷',
    "cuvee" -> '⋎',
    "cuwed" -> '⋏',
    "cwconint" -> '∲',
    "cwint " -> '∱',
    "cylcty " -> '⌭',
    "dagger " -> '†',
    "Dagger" -> '‡',
    "daleth " -> 'ℸ',
    "darr" -> '↓',
    "dArr" -> '⇓',
    "Darr " -> '↡',
    "dash " -> '‐',
    "dashv" -> '⊣',
    "dbkarow " -> '⤏',
    "dblac" -> '˝',
    "dcaron " -> 'ď',
    "Dcaron " -> 'Ď',
    "dcy " -> 'д',
    "Dcy " -> 'Д',
    "dd " -> 'ⅆ',
    "DD " -> 'ⅅ',
    "ddagger " -> '‡',
    "ddarr" -> '⇉',
    "DDotrahd " -> '⤑',
    "deg " -> '°',
    "Del " -> '∇',
    "delta " -> 'δ',
    "Delta " -> 'Δ',
    "demptyv " -> '⦱',
    "dfisht " -> '⥿',
    "dHar " -> '⥥',
    "dharl" -> '⇃',
    "dharr" -> '⇂',
    "DiacriticalAcute " -> '´',
    "DiacriticalDot " -> '˙',
    "DiacriticalDoubleAcute " -> '˝',
    "DiacriticalGrave " -> '`',
    "DiacriticalTilde " -> '˜',
    "diam" -> '⋄',
    "diamond" -> '⋄',
    "Diamond " -> '⋄',
    "diamondsuit " -> '♦',
    "diams" -> '♦',
    "die" -> '¨',
    "DifferentialD" -> 'ⅆ',
    "digamma " -> 'ϝ',
    "disin " -> '⋲',
    "divide " -> '÷',
    "divideontimes " -> '⋇',
    "divonx" -> '⋇',
    "djcy " -> 'ђ',
    "DJcy " -> 'Ђ',
    "dlcorn" -> '⌞',
    "dlcorner " -> '⌞',
    "dlcrop " -> '⌍',
    "dollar " -> '$',
    "dot" -> '˙',
    "Dot" -> '¨',
    "DotDot " -> '⃜',
    "doteq " -> '≐',
    "doteqdot " -> '≑',
    "DotEqual" -> '≐',
    "dotminus " -> '∸',
    "dotplus " -> '∔',
    "dotsquare " -> '⊡',
    "doublebarwedge " -> '⌆',
    "DoubleContourIntegral " -> '∯',
    "DoubleDot" -> '¨',
    "DoubleDownArrow " -> '⇓',
    "DoubleLeftArrow " -> '⇐',
    "DoubleLeftRightArrow" -> '⇔',
    "DoubleLongLeftArrow " -> '⟸',
    "DoubleLongLeftRightArrow " -> '⟺',
    "DoubleLongRightArrow " -> '⟹',
    "DoubleRightArrow " -> '⇒',
    "DoubleRightTee " -> '⊨',
    "DoubleUpArrow " -> '⇑',
    "DoubleUpDownArrow " -> '⇕',
    "DoubleVerticalBar" -> '∥',
    "downarrow" -> '↓',
    "Downarrow" -> '⇓',
    "DownArrow" -> '↓',
    "DownArrowBar " -> '⤓',
    "DownArrowUpArrow " -> '⇵',
    "DownBreve " -> '̑',
    "downdownarrows " -> '⇉',
    "downharpoonleft " -> '⇃',
    "downharpoonright " -> '⇂',
    "DownLeftRightVector " -> '⥐',
    "DownLeftTeeVector " -> '⥞',
    "DownLeftVector " -> '↽',
    "DownLeftVectorBar " -> '⥖',
    "DownRightTeeVector " -> '⥟',
    "DownRightVector " -> '⇁',
    "DownRightVectorBar " -> '⥗',
    "DownTee " -> '⊤',
    "DownTeeArrow" -> '↧',
    "drbkarow " -> '⤐',
    "drcorn" -> '⌟',
    "drcorner " -> '⌟',
    "drcrop " -> '⌌',
    "dscy " -> 'ѕ',
    "DScy " -> 'Ѕ',
    "dsol " -> '⧶',
    "dstrok " -> 'đ',
    "Dstrok " -> 'Đ',
    "dtdot " -> '⋱',
    "dtri" -> '▿',
    "dtrif" -> '▾',
    "duarr" -> '⇵',
    "duhar" -> '⥯',
    "dwangle " -> '⦦',
    "dzcy " -> 'џ',
    "DZcy " -> 'Џ',
    "dzigrarr " -> '⟿',
    "eacute " -> 'é',
    "Eacute " -> 'É',
    "ecaron " -> 'ě',
    "Ecaron " -> 'Ě',
    "ecir" -> '≖',
    "ecirc " -> 'ê',
    "Ecirc " -> 'Ê',
    "ecolon" -> '≕',
    "ecy " -> 'э',
    "Ecy " -> 'Э',
    "edot" -> '≑',
    "edot " -> 'ė',
    "Edot " -> 'Ė',
    "ee " -> 'ⅇ',
    "efdot" -> '≒',
    "egrave " -> 'è',
    "Egrave " -> 'È',
    "Element" -> '∈',
    "elinters " -> '⏧',
    "ell " -> 'ℓ',
    "emacr " -> 'ē',
    "Emacr " -> 'Ē',
    "empty" -> '∅',
    "emptyset" -> '∅',
    "EmptySmallSquare " -> '◻',
    "emptyv" -> '∅',
    "EmptyVerySmallSquare " -> '▫',
    "emsp " -> ' ',
    "emsp13 " -> ' ',
    "emsp14 " -> ' ',
    "eng " -> 'ŋ',
    "ENG " -> 'Ŋ',
    "ensp " -> ' ',
    "eogon " -> 'ę',
    "Eogon " -> 'Ę',
    "epar " -> '⋕',
    "eparsl " -> '⧣',
    "epsi" -> 'ϵ',
    "epsilon" -> 'ε',
    "Epsilon " -> 'Ε',
    "epsiv" -> 'ε',
    "eqcirc " -> '≖',
    "eqcolon " -> '≕',
    "eqsim " -> '≂',
    "equal " -> '=',
    "EqualTilde" -> '≂',
    "equest" -> '≟',
    "Equilibrium" -> '⇌',
    "equiv" -> '≡',
    "eqvparsl " -> '⧥',
    "erarr " -> '⥱',
    "erdot" -> '≓',
    "escr " -> 'ℯ',
    "Escr" -> 'ℰ',
    "esdot" -> '≐',
    "esim" -> '≂',
    "eta " -> 'η',
    "Eta " -> 'Η',
    "eth " -> 'ð',
    "ETH " -> 'Ð',
    "euml " -> 'ë',
    "Euml " -> 'Ë',
    "euro " -> '€',
    "excl " -> '!',
    "exist" -> '∃',
    "Exists " -> '∃',
    "expectation " -> 'ℰ',
    "exponentiale" -> 'ⅇ',
    "ExponentialE" -> 'ⅇ',
    "fallingdotseq " -> '≒',
    "fcy " -> 'ф',
    "Fcy " -> 'Ф',
    "female " -> '♀',
    "ffilig " -> 'ﬃ',
    "fflig " -> 'ﬀ',
    "ffllig " -> 'ﬄ',
    "filig " -> 'ﬁ',
    "FilledSmallSquare " -> '◼',
    "FilledVerySmallSquare " -> '▪',
    "flat " -> '♭',
    "fllig " -> 'ﬂ',
    "fltns " -> '▱',
    "fnof " -> 'ƒ',
    "forall" -> '∀',
    "ForAll " -> '∀',
    "fork" -> '⋔',
    "Fouriertrf " -> 'ℱ',
    "fpartint " -> '⨍',
    "frac12" -> '½',
    "frac13 " -> '⅓',
    "frac14 " -> '¼',
    "frac15 " -> '⅕',
    "frac16 " -> '⅙',
    "frac18 " -> '⅛',
    "frac23 " -> '⅔',
    "frac25 " -> '⅖',
    "frac34 " -> '¾',
    "frac35 " -> '⅗',
    "frac38 " -> '⅜',
    "frac45 " -> '⅘',
    "frac56 " -> '⅚',
    "frac58 " -> '⅝',
    "frac78 " -> '⅞',
    "frasl " -> '⁄',
    "frown" -> '⌢',
    "Fscr" -> 'ℱ',
    "gacute " -> 'ǵ',
    "gammad" -> 'ϝ',
    "Gammad " -> 'Ϝ',
    "gamma " -> 'γ',
    "Gamma " -> 'Γ',
    "gbreve " -> 'ğ',
    "Gbreve " -> 'Ğ',
    "Gcedil " -> 'Ģ',
    "gcirc " -> 'ĝ',
    "Gcirc " -> 'Ĝ',
    "gcy " -> 'г',
    "Gcy " -> 'Г',
    "gdot " -> 'ġ',
    "Gdot " -> 'Ġ',
    "ge" -> '≥',
    "gE" -> '≧',
    "gel" -> '⋛',
    "geq " -> '≥',
    "geqq " -> '≧',
    "gg " -> '≫',
    "Gg" -> '⋙',
    "ggg " -> '⋙',
    "gimel " -> 'ℷ',
    "gjcy " -> 'ѓ',
    "GJcy " -> 'Ѓ',
    "gl" -> '≷',
    "gnE" -> '≩',
    "gneqq " -> '≩',
    "gnsim " -> '⋧',
    "grave" -> '`',
    "GreaterEqual" -> '≥',
    "GreaterEqualLess " -> '⋛',
    "GreaterFullEqual" -> '≧',
    "GreaterLess " -> '≷',
    "GreaterTilde" -> '≳',
    "gscr " -> 'ℊ',
    "gsim" -> '≳',
    "gt" -> '>',
    "Gt" -> '≫',
    "GT " -> '>',
    "gtdot" -> '⋗',
    "gtlPar " -> '⦕',
    "gtrarr " -> '⥸',
    "gtrdot " -> '⋗',
    "gtreqless" -> '⋛',
    "gtrless" -> '≷',
    "gtrsimsim " -> '≳',
    "Hacek " -> 'ˇ',
    "hairsp" -> ' ',
    "half " -> '½',
    "hamilt" -> 'ℋ',
    "hardcy " -> 'ъ',
    "HARDcy " -> 'Ъ',
    "harr" -> '↔',
    "hArr" -> '⇔',
    "harrcir " -> '⥈',
    "harrw" -> '↭',
    "Hat " -> '^',
    "hbar" -> 'ℏ',
    "hcirc " -> 'ĥ',
    "Hcirc " -> 'Ĥ',
    "hearts" -> '♥',
    "heartsuit " -> '♥',
    "hellip" -> '…',
    "hercon " -> '⊹',
    "Hfr" -> 'ℌ',
    "HilbertSpace" -> 'ℋ',
    "hksearow " -> '⤥',
    "hkswarow " -> '⤦',
    "hoarr " -> '⇿',
    "homtht " -> '∻',
    "hookleftarrow " -> '↩',
    "hookrightarrow " -> '↪',
    "Hopf " -> 'ℍ',
    "horbar " -> '―',
    "HorizontalLine " -> '─',
    "Hscr " -> 'ℋ',
    "hslash " -> 'ℏ',
    "hstrok " -> 'ħ',
    "Hstrok " -> 'Ħ',
    "HumpDownHump" -> '≎',
    "Humpeq " -> '≏',
    "HumpEqual" -> '≏',
    "hybull " -> '⁃',
    "hyphen" -> '‐',
    "iacute " -> 'í',
    "Iacute " -> 'Í',
    "icirc " -> 'î',
    "Icirc " -> 'Î',
    "icy " -> 'и',
    "Icy " -> 'И',
    "Idot " -> 'İ',
    "iecy " -> 'е',
    "IEcy " -> 'Е',
    "iexcl " -> '¡',
    "iff " -> '⇔',
    "Ifr " -> 'ℑ',
    "igrave " -> 'ì',
    "Igrave " -> 'Ì',
    "ii " -> 'ⅈ',
    "iiiint " -> '⨌',
    "iiint " -> '∭',
    "iinfin " -> '⧜',
    "iiota " -> '℩',
    "ijlig " -> 'ĳ',
    "IJlig " -> 'Ĳ',
    "Im" -> 'ℑ',
    "imacr " -> 'ī',
    "Imacr " -> 'Ī',
    "image" -> 'ℑ',
    "ImaginaryI" -> 'ⅈ',
    "imagline " -> 'ℐ',
    "imagpart" -> 'ℑ',
    "imath" -> 'ı',
    "imof " -> '⊷',
    "imped " -> 'Ƶ',
    "Implies" -> '⇒',
    "in " -> '∈',
    "incare " -> '℅',
    "infin " -> '∞',
    "infintie " -> '⧝',
    "inodot " -> 'ı',
    "int" -> '∫',
    "intcal" -> '⊺',
    "integers" -> 'ℤ',
    "Integral " -> '∫',
    "intercal " -> '⊺',
    "Intersection" -> '⋂',
    "intlarhk " -> '⨗',
    "intprod " -> '⨼',
    "iocy " -> 'ё',
    "IOcy " -> 'Ё',
    "iogon " -> 'į',
    "Iogon " -> 'Į',
    "iota " -> 'ι',
    "Iota " -> 'Ι',
    "iprod" -> '⨼',
    "iquest " -> '¿',
    "Iscr" -> 'ℐ',
    "isin" -> '∈',
    "isindot " -> '⋵',
    "isinE " -> '⋹',
    "isins " -> '⋴',
    "isinsv " -> '⋳',
    "isinv" -> '∈',
    "itilde " -> 'ĩ',
    "Itilde " -> 'Ĩ',
    "iukcy " -> 'і',
    "Iukcy " -> 'І',
    "iuml " -> 'ï',
    "Iuml " -> 'Ï',
    "jcirc " -> 'ĵ',
    "Jcirc " -> 'Ĵ',
    "jcy " -> 'й',
    "Jcy " -> 'Й',
    "jmath " -> 'ȷ',
    "jsercy " -> 'ј',
    "Jsercy " -> 'Ј',
    "jukcy " -> 'є',
    "Jukcy " -> 'Є',
    "kappav" -> 'ϰ',
    "kappa " -> 'κ',
    "Kappa " -> 'Κ',
    "kcedil " -> 'ķ',
    "Kcedil " -> 'Ķ',
    "kcy " -> 'к',
    "Kcy " -> 'К',
    "kgreen " -> 'ĸ',
    "khcy " -> 'х',
    "KHcy " -> 'Х',
    "kjcy " -> 'ќ',
    "KJcy " -> 'Ќ',
    "lAarr" -> '⇚',
    "lacute " -> 'ĺ',
    "Lacute " -> 'Ĺ',
    "laemptyv " -> '⦳',
    "lagram" -> 'ℒ',
    "lambda " -> 'λ',
    "Lambda " -> 'Λ',
    "lang" -> '⟨',
    "Lang " -> '⟪',
    "langd " -> '⦑',
    "langle " -> '⟨',
    "Laplacetrf " -> 'ℒ',
    "laquo " -> '«',
    "larr" -> '←',
    "lArr" -> '⇐',
    "Larr" -> '↞',
    "larrb" -> '⇤',
    "larrbfs " -> '⤟',
    "larrfs " -> '⤝',
    "larrhk" -> '↩',
    "larrlp" -> '↫',
    "larrpl " -> '⤹',
    "larrsim " -> '⥳',
    "larrtl" -> '↢',
    "latail " -> '⤙',
    "lAtail " -> '⤛',
    "lbarr " -> '⤌',
    "lBarr " -> '⤎',
    "lbbrk " -> '❲',
    "lbrace " -> '{',
    "lbrack " -> '[',
    "lbrke " -> '⦋',
    "lbrksld " -> '⦏',
    "lbrkslu " -> '⦍',
    "lcaron " -> 'ľ',
    "Lcaron " -> 'Ľ',
    "lcedil " -> 'ļ',
    "Lcedil " -> 'Ļ',
    "lceil" -> '⌈',
    "lcub" -> '{',
    "lcy " -> 'л',
    "Lcy " -> 'Л',
    "ldca " -> '⤶',
    "ldquo" -> '“',
    "ldquor " -> '„',
    "ldrdhar " -> '⥧',
    "ldrushar " -> '⥋',
    "ldsh " -> '↲',
    "le" -> '≤',
    "lE" -> '≦',
    "LeftAngleBracket" -> '⟨',
    "leftarrow" -> '←',
    "Leftarrow" -> '⇐',
    "LeftArrow" -> '←',
    "LeftArrowBar " -> '⇤',
    "LeftArrowRightArrow " -> '⇆',
    "leftarrowtail " -> '↢',
    "LeftCeiling " -> '⌈',
    "LeftDoubleBracket " -> '⟦',
    "LeftDownTeeVector " -> '⥡',
    "LeftDownVector" -> '⇃',
    "LeftDownVectorBar " -> '⥙',
    "LeftFloor " -> '⌊',
    "leftharpoondown" -> '↽',
    "leftharpoonup " -> '↼',
    "leftleftarrows " -> '⇇',
    "leftrightarrow" -> '↔',
    "Leftrightarrow" -> '⇔',
    "LeftRightArrow " -> '↔',
    "leftrightarrows" -> '⇆',
    "leftrightharpoons " -> '⇋',
    "leftrightsquigarrow " -> '↭',
    "LeftRightVector " -> '⥎',
    "LeftTee " -> '⊣',
    "LeftTeeArrow" -> '↤',
    "LeftTeeVector " -> '⥚',
    "leftthreetimes " -> '⋋',
    "LeftTriangle " -> '⊲',
    "LeftTriangleBar " -> '⧏',
    "LeftTriangleEqual " -> '⊴',
    "LeftUpDownVector " -> '⥑',
    "LeftUpTeeVector " -> '⥠',
    "LeftUpVector " -> '↿',
    "LeftUpVectorBar " -> '⥘',
    "LeftVector" -> '↼',
    "LeftVectorBar " -> '⥒',
    "leg" -> '⋚',
    "leq " -> '≤',
    "leqq " -> '≦',
    "lessdot " -> '⋖',
    "lesseqgtr " -> '⋚',
    "LessEqual" -> '≤',
    "LessEqualGreater" -> '⋚',
    "LessFullEqual" -> '≦',
    "LessGreater " -> '≶',
    "lessgtr" -> '≶',
    "lesssim " -> '≲',
    "LessTilde" -> '≲',
    "lfisht " -> '⥼',
    "lfloor" -> '⌊',
    "lg" -> '≶',
    "lHar " -> '⥢',
    "lhard" -> '↽',
    "lharu" -> '↼',
    "lharul " -> '⥪',
    "lhblk " -> '▄',
    "ljcy " -> 'љ',
    "LJcy " -> 'Љ',
    "ll " -> '≪',
    "Ll" -> '⋘',
    "llarr" -> '⇇',
    "Lleftarrow " -> '⇚',
    "llhard " -> '⥫',
    "lll " -> '⋘',
    "lltri " -> '◺',
    "lmidot " -> 'ŀ',
    "Lmidot " -> 'Ŀ',
    "lmoust" -> '⎰',
    "lmoustache " -> '⎰',
    "lnE" -> '≨',
    "lneqq " -> '≨',
    "lnsim " -> '⋦',
    "loang " -> '⟬',
    "loarr " -> '⇽',
    "lobrk" -> '⟦',
    "longleftarrow" -> '⟵',
    "Longleftarrow" -> '⟸',
    "LongLeftArrow " -> '⟵',
    "longleftrightarrow" -> '⟷',
    "Longleftrightarrow" -> '⟺',
    "LongLeftRightArrow " -> '⟷',
    "longmapsto " -> '⟼',
    "longrightarrow" -> '⟶',
    "Longrightarrow" -> '⟹',
    "LongRightArrow " -> '⟶',
    "looparrowleft " -> '↫',
    "looparrowright " -> '↬',
    "lopar " -> '⦅',
    "loplus " -> '⨭',
    "lotimes " -> '⨴',
    "lowast " -> '∗',
    "lowbar " -> '_',
    "LowerLeftArrow " -> '↙',
    "LowerRightArrow " -> '↘',
    "loz" -> '◊',
    "lozenge " -> '◊',
    "lozf" -> '⧫',
    "lpar " -> '(',
    "lparlt " -> '⦓',
    "lrarr" -> '⇆',
    "lrhar" -> '⇋',
    "lrhard " -> '⥫',
    "lrtri " -> '⊿',
    "lsaquo " -> '‹',
    "Lscr" -> 'ℒ',
    "lsh" -> '↰',
    "Lsh " -> '↰',
    "lsim" -> '≲',
    "lsqb" -> '[',
    "lsquo" -> '‘',
    "lsquor " -> '‚',
    "lstrok " -> 'ł',
    "Lstrok " -> 'Ł',
    "lt" -> '<',
    "Lt" -> '≪',
    "LT " -> '<',
    "ltdot" -> '⋖',
    "lthree" -> '⋋',
    "ltimes " -> '⋉',
    "ltlarr " -> '⥶',
    "ltri" -> '◃',
    "ltrie" -> '⊴',
    "ltrif" -> '◂',
    "ltrPar " -> '⦖',
    "lurdshar " -> '⥊',
    "luruhar " -> '⥦',
    "macr" -> '¯',
    "male " -> '♂',
    "malt" -> '✠',
    "Maltese " -> '✠',
    "map" -> '↦',
    "Map " -> '⤅',
    "mapsto " -> '↦',
    "mapstodown " -> '↧',
    "mapstoleft " -> '↤',
    "mapstoup " -> '↥',
    "marker " -> '▮',
    "mcomma " -> '⨩',
    "mcy " -> 'м',
    "Mcy " -> 'М',
    "mdash " -> '—',
    "mDDot " -> '∺',
    "measuredangle " -> '∡',
    "MediumSpace " -> ' ',
    "Mellintrf " -> 'ℳ',
    "mho " -> '℧',
    "micro " -> 'µ',
    "mid" -> '∣',
    "midast " -> '*',
    "middot" -> '·',
    "minus " -> '−',
    "minusb" -> '⊟',
    "minusd" -> '∸',
    "minusdu " -> '⨪',
    "MinusPlus " -> '∓',
    "mldr " -> '…',
    "mmap" -> '⊸',
    "mnplus" -> '∓',
    "models " -> '⊧',
    "mp" -> '∓',
    "Mscr" -> 'ℳ',
    "mstpos " -> '∾',
    "multimap " -> '⊸',
    "mu " -> 'μ',
    "Mu " -> 'Μ',
    "nabla" -> '∇',
    "nacute " -> 'ń',
    "Nacute " -> 'Ń',
    "nap" -> '≉',
    "napos " -> 'ŉ',
    "napprox " -> '≉',
    "natur" -> '♮',
    "natural " -> '♮',
    "naturals " -> 'ℕ',
    "nbsp" -> ' ',
    "ncap " -> '⩃',
    "ncaron " -> 'ň',
    "Ncaron " -> 'Ň',
    "ncedil " -> 'ņ',
    "Ncedil " -> 'Ņ',
    "ncong" -> '≇',
    "ncup " -> '⩂',
    "ncy " -> 'н',
    "Ncy " -> 'Н',
    "ndash " -> '–',
    "ne" -> '≠',
    "nearhk " -> '⤤',
    "nearr" -> '↗',
    "neArr " -> '⇗',
    "nearrow " -> '↗',
    "nequiv" -> '≢',
    "nesear" -> '⤨',
    "NestedGreaterGreater" -> '≫',
    "NestedLessLess" -> '≪',
    "nexist" -> '∄',
    "nexists " -> '∄',
    "nge" -> '≱',
    "ngeq " -> '≱',
    "ngsim" -> '≵',
    "ngt" -> '≯',
    "ngtr " -> '≯',
    "nhArr" -> '⇍',
    "nharrow" -> '↮',
    "ni" -> '∋',
    "nis " -> '⋼',
    "nisd " -> '⋺',
    "niv" -> '∋',
    "njcy " -> 'њ',
    "NJcy " -> 'Њ',
    "nlarr" -> '↚',
    "nlArr" -> '⇍',
    "nldr " -> '‥',
    "nle" -> '≰',
    "nleftarrow " -> '↚',
    "nLeftArrow " -> '⇍',
    "nleftrightarrow " -> '↮',
    "nLeftrightarrow " -> '⇍',
    "nleq " -> '≰',
    "nless " -> '≮',
    "nlsim" -> '≴',
    "nlt" -> '≮',
    "nltri" -> '⋪',
    "nltrie" -> '⋬',
    "nmid" -> '∤',
    "Nopf" -> 'ℕ',
    "not " -> '¬',
    "NotCongruent " -> '≢',
    "NotCupCap " -> '≭',
    "NotDoubleVerticalBar" -> '∦',
    "NotElement" -> '∉',
    "NotEqual " -> '≠',
    "NotExists" -> '∄',
    "NotGreater" -> '≯',
    "NotGreaterEqual" -> '≱',
    "NotGreaterTilde " -> '≵',
    "notin" -> '∉',
    "notinva " -> '∉',
    "notinvb " -> '⋷',
    "notinvc " -> '⋶',
    "NotLeftTriangle " -> '⋪',
    "NotLeftTriangleEqual " -> '⋬',
    "NotLess" -> '≮',
    "NotLessEqual" -> '≰',
    "NotLessGreater " -> '≸',
    "NotLessTilde " -> '≴',
    "notni" -> '∌',
    "notniva" -> '∌',
    "notnivb " -> '⋾',
    "notnivc " -> '⋽',
    "NotPrecedes " -> '⊀',
    "NotPrecedesSlantEqual " -> '⋠',
    "NotReverseElement " -> '∌',
    "NotRightTriangle " -> '⋫',
    "NotRightTriangleEqual " -> '⋭',
    "NotSquareSubsetEqual " -> '⋢',
    "NotSquareSupersetEqual " -> '⋣',
    "NotSubsetEqual " -> '⊈',
    "NotSucceeds " -> '⊁',
    "NotSucceedsSlantEqual " -> '⋡',
    "NotSupersetEqual " -> '⊉',
    "NotTilde " -> '≁',
    "NotTildeEqual " -> '≄',
    "NotTildeFullEqual " -> '≇',
    "NotTildeTilde" -> '≉',
    "NotVerticalBar" -> '∤',
    "npar" -> '∦',
    "nparallel" -> '∦',
    "npolint " -> '⨔',
    "npr" -> '⊀',
    "nprcue" -> '⋠',
    "nprec" -> '⊀',
    "nrarr" -> '↛',
    "nrArr" -> '⇍',
    "nrightarrow " -> '↛',
    "nRightArrow " -> '⇍',
    "nrtri" -> '⋫',
    "nrtrie" -> '⋭',
    "nsc" -> '⊁',
    "nsccue" -> '⋡',
    "nshortmid " -> '∤',
    "nshortparallel " -> '∦',
    "nsim" -> '≁',
    "nsime" -> '≄',
    "nsimeq" -> '≄',
    "nsmid" -> '∤',
    "nspar" -> '∦',
    "nsqsube" -> '⋢',
    "nsqsupe" -> '⋣',
    "nsub " -> '⊄',
    "nsube" -> '⊈',
    "nsubseteq" -> '⊈',
    "nsucc" -> '⊁',
    "nsup " -> '⊅',
    "nsupe" -> '⊉',
    "nsupseteq" -> '⊉',
    "ntgl" -> '≹',
    "ntilde " -> 'ñ',
    "Ntilde " -> 'Ñ',
    "ntlg" -> '≸',
    "ntriangleleft" -> '⋪',
    "ntrianglelefteq" -> '⋬',
    "ntriangleright" -> '⋫',
    "ntrianglerighteq" -> '⋭',
    "num " -> '#',
    "numero " -> '№',
    "numsp " -> ' ',
    "nu " -> 'ν',
    "Nu " -> 'Ν',
    "nvdash " -> '⊬',
    "nvDash " -> '⊭',
    "nVdash " -> '⊮',
    "nVDash " -> '⊯',
    "nvHArr " -> '⤄',
    "nvinfin " -> '⧞',
    "nvlArr " -> '⤂',
    "nvrArr " -> '⤃',
    "nwarhk " -> '⤣',
    "nwarr" -> '↖',
    "nwArr " -> '⇖',
    "nwarrow " -> '↖',
    "nwnear " -> '⤧',
    "oacute " -> 'ó',
    "Oacute " -> 'Ó',
    "oast" -> '⊛',
    "ocirc " -> 'ô',
    "Ocirc " -> 'Ô',
    "ocr" -> '⊚',
    "ocy " -> 'о',
    "Ocy " -> 'О',
    "odash" -> '⊝',
    "odblac " -> 'ő',
    "Odblac " -> 'Ő',
    "odiv " -> '⨸',
    "odot" -> '⊙',
    "odsold " -> '⦼',
    "oelig " -> 'œ',
    "OElig " -> 'Œ',
    "ofcir " -> '⦿',
    "ogon " -> '˛',
    "ograve " -> 'ò',
    "Ograve " -> 'Ò',
    "ogt " -> '⧁',
    "ohbar " -> '⦵',
    "ohm " -> 'Ω',
    "oint" -> '∮',
    "olarr" -> '↺',
    "olcir " -> '⦾',
    "olcross " -> '⦻',
    "oline " -> '‾',
    "olt " -> '⧀',
    "omacr " -> 'ō',
    "Omacr " -> 'Ō',
    "omega " -> 'ω',
    "Omega " -> 'Ω',
    "omicron " -> 'ο',
    "Omicron " -> 'Ο',
    "omid " -> '⦶',
    "ominus" -> '⊖',
    "opar " -> '⦶',
    "OpenCurlyDoubleQuote " -> '“',
    "OpenCurlyQuote " -> '‘',
    "operp " -> '⦹',
    "oplus" -> '⊕',
    "or" -> '∨',
    "Or " -> '⩔',
    "orarr" -> '↻',
    "ord " -> '⩝',
    "order" -> 'ℴ',
    "orderof" -> 'ℴ',
    "ordf " -> 'ª',
    "ordm " -> 'º',
    "origof " -> '⊶',
    "oror " -> '⩕',
    "orslope " -> '⩗',
    "orv " -> '⩛',
    "oS" -> 'Ⓢ',
    "oscr " -> 'ℴ',
    "oslash " -> 'ø',
    "Oslash " -> 'Ø',
    "osol " -> '⊘',
    "otilde " -> 'õ',
    "Otilde " -> 'Õ',
    "otimes" -> '⊗',
    "Otimes " -> '⨷',
    "otimesas " -> '⨶',
    "ouml " -> 'ö',
    "Ouml " -> 'Ö',
    "ovbar " -> '⌽',
    "OverBar" -> '¯',
    "OverBrace " -> '⏞',
    "OverBracket " -> '⎴',
    "OverParenthesis " -> '⏜',
    "par" -> '∥',
    "para " -> '¶',
    "parallel" -> '∥',
    "part" -> '∂',
    "PartialD " -> '∂',
    "pcy " -> 'п',
    "Pcy " -> 'П',
    "percnt " -> '%',
    "period " -> '.',
    "permil " -> '‰',
    "perp" -> '⊥',
    "pertenk " -> '‱',
    "phi " -> 'φ',
    "Phi " -> 'Φ',
    "phmmat" -> 'ℳ',
    "phone " -> '☎',
    "pitchfork " -> '⋔',
    "piv" -> 'ϖ',
    "pi " -> 'π',
    "Pi " -> 'Π',
    "planck" -> 'ℏ',
    "planckh " -> 'ℎ',
    "plankv" -> 'ℏ',
    "plusacir " -> '⨣',
    "plusb" -> '⊞',
    "pluscir " -> '⨢',
    "plusdo" -> '∔',
    "plusdu " -> '⨥',
    "PlusMinus " -> '±',
    "plusmn" -> '±',
    "plussim " -> '⨦',
    "plustwo " -> '⨧',
    "pm" -> '±',
    "PoincarePlane " -> 'ℌ',
    "pointint " -> '⨕',
    "Popf" -> 'ℙ',
    "pound " -> '£',
    "pr" -> '≺',
    "prcue" -> '≼',
    "prec " -> '≺',
    "preccurlyeq " -> '≼',
    "Precedes" -> '≺',
    "PrecedesSlantEqual" -> '≼',
    "PrecedesTilde " -> '≾',
    "precnsim " -> '⋨',
    "precsim" -> '≾',
    "prime " -> '′',
    "Prime " -> '″',
    "primes " -> 'ℙ',
    "prnsim" -> '⋨',
    "prod" -> '∏',
    "Product " -> '∏',
    "profalar " -> '⌮',
    "profline " -> '⌒',
    "profsurf " -> '⌓',
    "prop" -> '∝',
    "Proportion " -> '∷',
    "Proportional" -> '∝',
    "propto" -> '∝',
    "prsim" -> '≾',
    "prurel " -> '⊰',
    "psi " -> 'ψ',
    "Psi " -> 'Ψ',
    "puncsp " -> ' ',
    "qint" -> '⨌',
    "Qopf " -> 'ℚ',
    "qprime " -> '⁗',
    "quaternions" -> 'ℍ',
    "quatint " -> '⨖',
    "quest " -> '?',
    "questeq " -> '≟',
    "quot" -> '"',
    "QUOT " -> '"',
    "rAarr" -> '⇛',
    "race " -> '⧚',
    "racute " -> 'ŕ',
    "Racute " -> 'Ŕ',
    "radic" -> '√',
    "raemptyv " -> '⦳',
    "rang" -> '⟩',
    "Rang " -> '⟫',
    "rangd " -> '⦒',
    "range " -> '⦥',
    "rangle " -> '⟩',
    "raquo " -> '»',
    "rarr" -> '→',
    "rArr" -> '⇒',
    "Rarr" -> '↠',
    "rarrap " -> '⥵',
    "rarrb" -> '⇤',
    "rarrbfs " -> '⤠',
    "rarrc " -> '⤳',
    "rarrfs " -> '⤞',
    "rarrhk" -> '↪',
    "rarrlp" -> '↬',
    "rarrpl " -> '⥅',
    "rarrsim " -> '⥴',
    "rarrtl" -> '↣',
    "Rarrtl " -> '⤖',
    "rarrw" -> '↝',
    "ratail " -> '⤚',
    "rAtail " -> '⤜',
    "ratio " -> '∶',
    "rationals" -> 'ℚ',
    "rbarr" -> '⤍',
    "rBarr" -> '⤏',
    "RBarr" -> '⤐',
    "rbbrk " -> '❳',
    "rbrace " -> '}',
    "rbrack " -> ']',
    "rbrke " -> '⦌',
    "rbrksld " -> '⦎',
    "rbrkslu " -> '⦐',
    "rcaron " -> 'ř',
    "Rcaron " -> 'Ř',
    "rcedil " -> 'ŗ',
    "Rcedil " -> 'Ŗ',
    "rceil" -> '⌉',
    "rcub" -> '}',
    "rcy " -> 'р',
    "Rcy " -> 'Р',
    "rdca " -> '⤷',
    "rdldhar " -> '⥩',
    "rdquo" -> '”',
    "rdquor" -> '”',
    "rdsh " -> '↳',
    "Re" -> 'ℜ',
    "real" -> 'ℜ',
    "realine " -> 'ℛ',
    "realpart" -> 'ℜ',
    "reals" -> 'ℝ',
    "rect " -> '▭',
    "reg" -> '®',
    "REG " -> '®',
    "ReverseElement" -> '∋',
    "ReverseEquilibrium" -> '⇋',
    "ReverseUpEquilibrium " -> '⥯',
    "rfisht " -> '⥽',
    "rfloor" -> '⌋',
    "Rfr " -> 'ℜ',
    "rHar " -> '⥤',
    "rhard" -> '⇁',
    "rharu" -> '⇀',
    "rharul " -> '⥬',
    "rhov" -> 'ϱ',
    "rho " -> 'ρ',
    "Rho " -> 'Ρ',
    "RightAngleBracket" -> '⟩',
    "rightarrow" -> '→',
    "Rightarrow" -> '⇒',
    "RightArrow" -> '→',
    "RightArrowBar " -> '⇤',
    "RightArrowLeftArrow " -> '⇄',
    "rightarrowtail " -> '↣',
    "RightCeiling " -> '⌉',
    "RightDoubleBracket " -> '⟧',
    "RightDownTeeVector " -> '⥝',
    "RightDownVector" -> '⇂',
    "RightDownVectorBar " -> '⥕',
    "RightFloor " -> '⌋',
    "rightharpoondown" -> '⇁',
    "rightharpoonup " -> '⇀',
    "rightleftarrows" -> '⇄',
    "rightleftharpoons " -> '⇌',
    "rightrightarrows " -> '⇉',
    "rightsquigarrow " -> '↝',
    "RightTee " -> '⊢',
    "RightTeeArrow" -> '↦',
    "RightTeeVector " -> '⥛',
    "rightthreetimes " -> '⋌',
    "RightTriangle; " -> '⊳',
    "RightTriangleBar " -> '⧐',
    "RightTriangleEqual " -> '⊵',
    "RightUpDownVector " -> '⥏',
    "RightUpTeeVector " -> '⥜',
    "RightUpVector " -> '↾',
    "RightUpVectorBar " -> '⥔',
    "RightVector" -> '⇀',
    "RightVectorBar " -> '⥓',
    "ring " -> '˚',
    "risingdotseq " -> '≓',
    "rlarr" -> '⇄',
    "rlhar" -> '⇌',
    "rmoust" -> '⎱',
    "rmoustache " -> '⎱',
    "roang " -> '⟭',
    "roarr " -> '⇾',
    "robrk" -> '⟧',
    "ropar " -> '⦆',
    "Ropf " -> 'ℝ',
    "roplus " -> '⨮',
    "rotimes " -> '⨵',
    "RoundImplies " -> '⥰',
    "rpar " -> ')',
    "rpargt " -> '⦔',
    "rppolint " -> '⨒',
    "rrarr" -> '⇉',
    "Rrightarrow " -> '⇛',
    "rsaquo " -> '›',
    "Rscr" -> 'ℛ',
    "rsh" -> '↱',
    "Rsh " -> '↱',
    "rsqb" -> ']',
    "rsquo" -> '’',
    "rsquor" -> '’',
    "rthree" -> '⋌',
    "rtimes " -> '⋊',
    "rtri" -> '▹',
    "rtrie" -> '⊵',
    "rtrif" -> '▸',
    "rtriltri " -> '⧎',
    "RuleDelayed " -> '⧴',
    "ruluhar " -> '⥨',
    "rx " -> '℞',
    "sacute " -> 'ś',
    "Sacute " -> 'Ś',
    "sbquo" -> '‚',
    "sc" -> '≻',
    "scaron " -> 'š',
    "Scaron " -> 'Š',
    "sccue" -> '≽',
    "scedil " -> 'ş',
    "Scedil " -> 'Ş',
    "scirc " -> 'ŝ',
    "Scirc " -> 'Ŝ',
    "scnsim" -> '⋩',
    "scpolint " -> '⨓',
    "scy " -> 'с',
    "Scy " -> 'С',
    "sdot " -> '⋅',
    "sdotb" -> '⊡',
    "searhk" -> '⤥',
    "searr" -> '↘',
    "seArr " -> '⇘',
    "searrow" -> '↘',
    "sect " -> '§',
    "semi " -> ';',
    "seswar" -> '⤩',
    "setminus" -> '∖',
    "setmn" -> '∖',
    "sext " -> '✶',
    "sfrown " -> '⌢',
    "sharp " -> '♯',
    "shchcy " -> 'щ',
    "SHCHcy " -> 'Щ',
    "shcy " -> 'ш',
    "SHcy " -> 'Ш',
    "ShortDownArrow " -> '↓',
    "ShortLeftArrow " -> '←',
    "shortmid " -> '∣',
    "shortparallel " -> '∥',
    "ShortRightArrow " -> '→',
    "ShortUpArrow " -> '↑',
    "sigmaf" -> 'ς',
    "sigmav" -> 'ς',
    "sigma " -> 'σ',
    "Sigma " -> 'Σ',
    "sim" -> '∼',
    "sime" -> '≃',
    "simeq " -> '≃',
    "simne " -> '≆',
    "simplus " -> '⨤',
    "simrarr " -> '⥲',
    "slarr" -> '←',
    "SmallCircle " -> '∘',
    "smallsetminus " -> '∖',
    "smashp " -> '⨳',
    "smeparsl " -> '⧤',
    "smid" -> '∣',
    "smile" -> '⌣',
    "softcy " -> 'ь',
    "SOFTcy " -> 'Ь',
    "sol " -> '/',
    "solb " -> '⧄',
    "solbar " -> '⌿',
    "spades" -> '♠',
    "spadesuit " -> '♠',
    "spar" -> '∥',
    "sqcap" -> '⊓',
    "sqcup" -> '⊔',
    "Sqrt " -> '√',
    "sqsub" -> '⊏',
    "sqsube" -> '⊑',
    "sqsubset " -> '⊏',
    "sqsubseteq " -> '⊑',
    "sqsup" -> '⊐',
    "sqsupe" -> '⊒',
    "sqsupset " -> '⊐',
    "sqsupseteq " -> '⊒',
    "squ" -> '□',
    "square" -> '□',
    "Square " -> '□',
    "squaref" -> '▪',
    "SquareIntersection " -> '⊓',
    "SquareSubset" -> '⊏',
    "SquareSubsetEqual" -> '⊑',
    "SquareSuperset" -> '⊐',
    "SquareSupersetEqual" -> '⊒',
    "SquareUnion " -> '⊔',
    "squf" -> '▪',
    "srarr" -> '→',
    "ssetmn" -> '∖',
    "ssmile " -> '⌣',
    "sstarf" -> '⋆',
    "star " -> '☆',
    "Star " -> '⋆',
    "starf" -> '★',
    "straightepsilon " -> 'ϵ',
    "straightphi " -> 'ϕ',
    "strns " -> '¯',
    "sub" -> '⊂',
    "Sub" -> '⋐',
    "sube" -> '⊆',
    "subne" -> '⊊',
    "subrarr " -> '⥹',
    "subset " -> '⊂',
    "Subset " -> '⋐',
    "subseteq " -> '⊆',
    "SubsetEqual" -> '⊆',
    "subsetneq " -> '⊊',
    "succ " -> '≻',
    "succcurlyeq " -> '≽',
    "Succeeds" -> '≻',
    "SucceedsSlantEqual" -> '≽',
    "succnsim " -> '⋩',
    "SuchThat " -> '∋',
    "sum" -> '∑',
    "Sum " -> '∑',
    "sung " -> '♪',
    "sup" -> '⊃',
    "Sup" -> '⋑',
    "sup1 " -> '¹',
    "sup2 " -> '²',
    "sup3 " -> '³',
    "supe" -> '⊇',
    "Superset " -> '⊃',
    "SupersetEqual " -> '⊇',
    "supne" -> '⊋',
    "suprarr " -> '⥻',
    "supset" -> '⊃',
    "Supset " -> '⋑',
    "supseteq" -> '⊇',
    "supsetneq " -> '⊋',
    "swarhk" -> '⤦',
    "swarr" -> '↙',
    "swArr " -> '⇙',
    "swarrow" -> '↙',
    "swnwar " -> '⤪',
    "szlig " -> 'ß',
    "target " -> '⌖',
    "tau " -> 'τ',
    "Tau " -> 'Τ',
    "tbrk" -> '⎴',
    "tcaron " -> 'ť',
    "Tcaron " -> 'Ť',
    "tcedil " -> 'ţ',
    "Tcedil " -> 'Ţ',
    "tcy " -> 'т',
    "Tcy " -> 'Т',
    "tdot" -> '⃛',
    "telrec " -> '⌕',
    "there4" -> '∴',
    "therefore" -> '∴',
    "Therefore " -> '∴',
    "thetasym" -> 'ϑ',
    "thetav" -> 'ϑ',
    "theta " -> 'θ',
    "Theta " -> 'Θ',
    "thickapprox " -> '≈',
    "thicksim " -> '∼',
    "thinsp" -> ' ',
    "ThinSpace " -> ' ',
    "thkap" -> '≈',
    "thksim" -> '∼',
    "thorn " -> 'þ',
    "THORN " -> 'Þ',
    "tilde" -> '˜',
    "Tilde" -> '∼',
    "TildeEqual" -> '≃',
    "TildeFullEqual " -> '≅',
    "TildeTilde" -> '≈',
    "times " -> '×',
    "timesb" -> '⊠',
    "timesbar " -> '⨱',
    "timesd " -> '⨰',
    "tint" -> '∭',
    "toea " -> '⤨',
    "top" -> '⊤',
    "topbot " -> '⌶',
    "tosa " -> '⤩',
    "tprime " -> '‴',
    "trade " -> '™',
    "triangle " -> '▵',
    "triangledown " -> '▿',
    "triangleleft " -> '◃',
    "trianglelefteq" -> '⊴',
    "triangleq " -> '≜',
    "triangleright " -> '▹',
    "trianglerighteq" -> '⊵',
    "tridot " -> '◬',
    "trie" -> '≜',
    "triminus " -> '⨺',
    "TripleDot " -> '⃛',
    "triplus " -> '⨹',
    "trisb " -> '⧍',
    "tritime " -> '⨻',
    "trpezium " -> '⏢',
    "tscy " -> 'ц',
    "TScy " -> 'Ц',
    "tshcy " -> 'ћ',
    "TSHcy " -> 'Ћ',
    "tstrok " -> 'ŧ',
    "Tstrok " -> 'Ŧ',
    "twixt" -> '≬',
    "twoheadleftarrow " -> '↞',
    "twoheadrightarrow " -> '↠',
    "uacute " -> 'ú',
    "Uacute " -> 'Ú',
    "uarr" -> '↑',
    "uArr" -> '⇑',
    "Uarr " -> '↟',
    "Uarrocir " -> '⥉',
    "ubrcy " -> 'ў',
    "Ubrcy " -> 'Ў',
    "ubreve " -> 'ŭ',
    "Ubreve " -> 'Ŭ',
    "ucirc " -> 'û',
    "Ucirc " -> 'Û',
    "ucy " -> 'у',
    "Ucy " -> 'У',
    "udarr" -> '⇅',
    "udblac " -> 'ű',
    "Udblac " -> 'Ű',
    "udhar" -> '⥮',
    "ufisht " -> '⥾',
    "ugrave " -> 'ù',
    "Ugrave " -> 'Ù',
    "uHar " -> '⥣',
    "uharl" -> '↿',
    "uharr" -> '↾',
    "uhblk " -> '▀',
    "ulcorn" -> '⌜',
    "ulcorner " -> '⌜',
    "ulcrop " -> '⌏',
    "ultri " -> '◸',
    "umacr " -> 'ū',
    "Umacr " -> 'Ū',
    "uml " -> '¨',
    "UnderBar " -> '̲',
    "UnderBrace " -> '⏟',
    "UnderBracket " -> '⎵',
    "UnderParenthesis " -> '⏝',
    "Union" -> '⋃',
    "UnionPlus " -> '⊎',
    "uogon " -> 'ų',
    "Uogon " -> 'Ų',
    "uparrow" -> '↑',
    "Uparrow" -> '⇑',
    "UpArrow" -> '↑',
    "UpArrowBar " -> '⤒',
    "UpArrowDownArrow " -> '⇅',
    "updownarrow" -> '↕',
    "Updownarrow" -> '⇕',
    "UpDownArrow " -> '↕',
    "UpEquilibrium " -> '⥮',
    "upharpoonleft" -> '↿',
    "upharpoonright" -> '↾',
    "uplus" -> '⊎',
    "UpperLeftArrow" -> '↖',
    "UpperRightArrow" -> '↗',
    "upsih" -> 'ϒ',
    "upsilon" -> 'υ',
    "Upsilon " -> 'Υ',
    "upsi " -> 'υ',
    "Upsi " -> 'ϒ',
    "UpTee " -> '⊥',
    "UpTeeArrow" -> '↥',
    "upuparrows " -> '⇈',
    "urcorn" -> '⌝',
    "urcorner " -> '⌝',
    "urcrop " -> '⌎',
    "uring " -> 'ů',
    "Uring " -> 'Ů',
    "urtri " -> '◹',
    "utdot " -> '⋰',
    "utilde " -> 'ũ',
    "Utilde " -> 'Ũ',
    "utri" -> '▵',
    "utrif" -> '▴',
    "uuarr" -> '⇈',
    "uuml " -> 'ü',
    "Uuml " -> 'Ü',
    "uwangle " -> '⦧',
    "vangrt " -> '⦜',
    "varepsilon " -> 'ε',
    "varkappa " -> 'ϰ',
    "varnothing " -> '∅',
    "varpi " -> 'ϖ',
    "varpropto " -> '∝',
    "varr" -> '↕',
    "vArr" -> '⇕',
    "varrho " -> 'ϱ',
    "varsigma " -> 'ς',
    "vartheta " -> 'ϑ',
    "vartriangleleft" -> '⊲',
    "vartriangleright" -> '⊳',
    "vcy " -> 'в',
    "Vcy " -> 'В',
    "vdash" -> '⊢',
    "vDash" -> '⊨',
    "Vdash " -> '⊩',
    "VDash " -> '⊫',
    "vee " -> '∨',
    "Vee" -> '⋁',
    "veebar " -> '⊻',
    "veeeq " -> '≚',
    "vellip " -> '⋮',
    "verbar" -> '|',
    "Verbar" -> '‖',
    "vert" -> '|',
    "Vert " -> '‖',
    "VerticalBar" -> '∣',
    "VerticalLine " -> '|',
    "VerticalSeparator " -> '❘',
    "VerticalTilde" -> '≀',
    "VeryThinSpace " -> ' ',
    "vltri" -> '⊲',
    "vprop" -> '∝',
    "vrtri" -> '⊳',
    "Vvdash " -> '⊪',
    "vzigzag " -> '⦚',
    "wcirc " -> 'ŵ',
    "Wcirc " -> 'Ŵ',
    "wedbar " -> '⩟',
    "wedge " -> '∧',
    "Wedge" -> '⋀',
    "wedgeq " -> '≙',
    "weierp" -> '℘',
    "wp " -> '℘',
    "wr " -> '≀',
    "wreath" -> '≀',
    "xcap" -> '⋂',
    "xcirc" -> '◯',
    "xcup" -> '⋃',
    "xdtri" -> '▽',
    "xharr" -> '⟷',
    "xhArr" -> '⟺',
    "xi " -> 'ξ',
    "Xi " -> 'Ξ',
    "xlarr" -> '⟵',
    "xlArr" -> '⟸',
    "xmap" -> '⟼',
    "xnis " -> '⋻',
    "xodot" -> '⨀',
    "xoplus" -> '⨁',
    "xotime" -> '⨂',
    "xrarr" -> '⟶',
    "xrArr" -> '⟹',
    "xsqcup" -> '⨆',
    "xuplus" -> '⨄',
    "xutri" -> '△',
    "xvee" -> '⋁',
    "xwedge" -> '⋀',
    "yacute " -> 'ý',
    "Yacute " -> 'Ý',
    "yacy " -> 'я',
    "YAcy " -> 'Я',
    "ycirc " -> 'ŷ',
    "Ycirc " -> 'Ŷ',
    "ycy " -> 'ы',
    "Ycy " -> 'Ы',
    "yen " -> '¥',
    "yicy " -> 'ї',
    "YIcy " -> 'Ї',
    "yucy " -> 'ю',
    "YUcy " -> 'Ю',
    "yuml " -> 'ÿ',
    "Yuml " -> 'Ÿ',
    "zacute " -> 'ź',
    "Zacute " -> 'Ź',
    "zcaron " -> 'ž',
    "Zcaron " -> 'Ž',
    "zcy " -> 'з',
    "Zcy " -> 'З',
    "zdot " -> 'ż',
    "Zdot " -> 'Ż',
    "zeetrf " -> 'ℨ',
    "zeta " -> 'ζ',
    "Zeta " -> 'Ζ',
    "Zfr" -> 'ℨ',
    "zhcy " -> 'ж',
    "ZHcy " -> 'Ж',
    "zigrarr " -> '⇝',
    "Zopf " -> 'ℤ'
  )

}
