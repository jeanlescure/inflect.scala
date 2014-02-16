//Inflect Scala Object
//@author: Jean M. Lescure
//Based on Rails' ActiveSupport::Inflector::Inflections class
//Has methods for returning either the plural or singular forms 
//of words from the English language
object inflect {
  def plural(str: String):String = str match {
    case x if x.matches("$") => "s"
    case x if x.matches("^(?i:s)$") => x
    case x if x.matches(".*?(?i:fish|rice|police)$") => x
    case x if x.matches(".*?(?i:person)$") => replaceAndRespectCase(x,"(.*?)((?i:person))$","people")
    case x if x.matches(".*?(?i:man)$") => replaceAndRespectCase(x,"(.*?)((?i:man))$","men")
    case x if x.matches(".*?(?i:child)$") => replaceAndRespectCase(x,"(.*?(?i:child))(.*?)$","ren")
    case x if x.matches(".*?(?i:sex)$") => replaceAndRespectCase(x,"(.*?(?i:sex))(.*?)$","es")
    case x if x.matches(".*?(?i:move)$") => replaceAndRespectCase(x,"(.*?(?i:move))(.*?)$","s")
    case x if x.matches(".*?(?i:cow)$") => replaceAndRespectCase(x,"(.*?)((?i:cow))$","kine")
    case x if x.matches(".*?(?i:zombie)$") => replaceAndRespectCase(x,"(.*?(?i:zombie))(.*?)$","s")
    case x if x.matches("^(?i:ox)$") => replaceAndRespectCase(x,"^((?i:ox))(.*?)$","en")
    case x if x.matches("^(?i:oxen)$") => x
    case x if x.matches("^(?i:qualia)$") => x
    case x if x.matches(".*?(?i:ax|test)(?i:is)$") => replaceAndRespectCase(x,"^(.*?(?i:ax|test))((?i:is))$","es")
    case x if x.matches(".*?(?i:octop|vir)(?i:us)$") => replaceAndRespectCase(x,"(.*?(?i:octop|vir))((?i:us))$","i")
    case x if x.matches(".*?(?i:octop|vir)(?i:i)$") => replaceAndRespectCase(x,"(.*?(?i:octop|vir))((?i:i))$","i")
    case x if x.matches(".*?(?i:alias|status)$") => replaceAndRespectCase(x,"^(.*?(?i:alias|status))(.*?)$","es")case x if x.matches(".*?(?i:bu)(?i:s)$") => replaceAndRespectCase(x,"(.*?(?i:bu))((?i:s))$","es")
    case x if x.matches(".*?(?i:buffal|tomat)(?i:o)$") => replaceAndRespectCase(x,"(.*?(?i:buffal|tomat))((?i:o))$","oes")
    case x if x.matches(".*?(?i:[ti])(?i:um)$") => replaceAndRespectCase(x,"(.*?(?i:[ti]))((?i:um))$","a")
    case x if x.matches(".*?(?i:[ti]a)$") => x
    case x if x.matches(".*?(?i:sis)$") => replaceAndRespectCase(x,"(.*?)((?i:sis))$","ses")
    case x if x.matches(".*?([^fF])(?i:fe)$") => replaceAndRespectCase(x,"(.*?([^fF]))((?i:fe))$","ves")
    case x if x.matches(".*?([lrLR])(?i:f)$") => replaceAndRespectCase(x,"(.*?([lrLR]))((?i:f))$","ves")
    case x if x.matches(".*?(?i:hive)$") => replaceAndRespectCase(x,"^(.*?(?i:hive))(.*?)$","s")
    case x if x.matches(".*?(?i:[^aeiouy]|qu)(?i:y)$") => replaceAndRespectCase(x,"(.*?(?i:[^aeiouy]|qu))((?i:y))$","ies")
    case x if x.matches(".*?(?i:matr|vert|ind)(?i:ix|ex)$") => replaceAndRespectCase(x,"(.*?(?i:matr|vert|ind))((?i:ix|ex))$","ices")
    case x if x.matches(".*?(?i:x|ch|ss|sh)$") => replaceAndRespectCase(x,"^(.*?(?i:x|ch|ss|sh))(.*?)$","es")
    case x if x.matches(".*?(?i:m|l)(?i:ouse)$") => replaceAndRespectCase(x,"(.*?(?i:m|l))((?i:ouse))$","ice")
    case x if x.matches("^(?i:lice)$") => x
    case x if x.matches(".*?(?i:lice)$") => replaceAndRespectCase(x,"^(.*?(?i:lice))(.*?)$","s")
    case x if x.matches(".*?(?i:m)(?i:ice)$") => x
    case x if x.matches(".*?(?i:ce)$") => replaceAndRespectCase(x,"^(.*?(?i:ce))(.*?)$","s")
    case x if x.matches(".*?(?i:quiz)$") => replaceAndRespectCase(x,"^(.*?(?i:quiz))(.*?)$","zes")
    case x if x.matches(".*?(?i:ase)$") => replaceAndRespectCase(x,"^(.*(?i:ase))(.*?)$","s")
    case x if x.matches(".*?(?i:[^aeious])$") => replaceAndRespectCase(x,"^(.*(?i:[^aeious]))(.*?)$","s")
    case x => x
  }
  def singular(str: String):String = str match {
    case x if x.matches("$") => ""
    case x if x.matches(".*?(?i:ss)$") => x
    case x if x.matches(".*?(?i:people)$") => replaceAndRespectCase(x,"(.*?)((?i:people))$","person")
    case x if x.matches(".*?(?i:men)$") => replaceAndRespectCase(x,"(.*?)((?i:men))$","man")
    case x if x.matches(".*?(?i:children)$") => replaceAndRespectCase(x,"(.*?(?i:child))((?i:ren))$","")
    case x if x.matches(".*?(?i:sexes)$") => replaceAndRespectCase(x,"(.*?(?i:sex))((?i:es))$","")
    case x if x.matches(".*?(?i:moves)$") => replaceAndRespectCase(x,"(.*?(?i:move))((?i:s))$","")
    case x if x.matches(".*?(?i:kine)$") => replaceAndRespectCase(x,"(.*?)((?i:kine))$","cow")
    case x if x.matches(".*?(?i:zombies)$") => replaceAndRespectCase(x,"(.*?(?i:zombie))((?i:s))$","")
    case x if x.matches("^(?i:news)$") => x
    case x if x.matches("(.*?(?i:[ye])){0,1}(?i:oxen)$") => replaceAndRespectCase(x,"(.*?(?i:ox))((?i:en))$","")
    case x if x.matches(".*?(?i:[ti])(?i:a)$") => replaceAndRespectCase(x,"(.*?(?i:[ti]))((?i:a))$","um")
    case x if x.matches(".*?(?i:database)(?i:s)$") => replaceAndRespectCase(x,"(.*?(?i:database))((?i:s))$","")
    case x if x.matches(".*(?i:analy|ba|diagno|parenthe|progno|synop|the)(?i:sis|ses)$") => replaceAndRespectCase(x,"(.*?(?i:analy|ba|diagno|parenthe|progno|synop|the))((?i:sis|ses))$","sis")
    case x if x.matches(".*?(?i:hive|tive)(?i:s)$") => replaceAndRespectCase(x,"(.*?(?i:hive|tive))((?i:s))$","")
    case x if x.matches(".*?(?i:[lr])(?i:ves)$") => replaceAndRespectCase(x,"(.*?(?i:[lr]))((?i:ves))$","f")
    case x if x.matches(".*?(?i:[^f])(?i:ves)$") => replaceAndRespectCase(x,"(.*?(?i:[^f]))((?i:ves))$","fe")
    case x if x.matches(".*?(?i:series)$") => x
    case x if x.matches(".*?(?i:movie)(?i:s)$") => replaceAndRespectCase(x,"(.*?(?i:movie))((?i:s))$","")
    case x if x.matches(".*?(?i:[^aeiouy]|qu)(?i:ies)$") => replaceAndRespectCase(x,"(.*?(?i:[^aeiouy]|qu))((?i:ies))$","y")
    case x if x.matches(".*?(?i:ax)(?i:[ie]s)$") => replaceAndRespectCase(x,"(.*?(?i:ax))((?i:[ie]s))$","is")
    case x if x.matches(".*?(?i:x|ch|ss|sh)(?i:es)$") => replaceAndRespectCase(x,"(.*?(?i:x|ch|ss|sh))((?i:es))$","")
    case x if x.matches(".*?(?i:mice)$") => replaceAndRespectCase(x,"(.*?(?i:m))((?i:ice))$","ouse")
    case x if x.matches("(.*?(?i:[ye])){0,1}(?i:lice)$") => replaceAndRespectCase(x,"(.*?(?i:l))((?i:ice))$","ouse")
    case x if x.matches(".*?(?i:lice)(?i:s)$") => replaceAndRespectCase(x,"(.*?(?i:lice))((?i:s))$","")
    case x if x.matches(".*?(?i:vert|ind)(?i:ices)$") => replaceAndRespectCase(x,"(.*?(?i:vert|ind))((?i:ices))$","ex")
    case x if x.matches(".*?(?i:matr)(?i:ices)$") => replaceAndRespectCase(x,"(.*?(?i:matr))((?i:ices))$","ix")
    case x if x.matches(".*?(?i:ces)$") => replaceAndRespectCase(x,"(.*?(?i:ce))((?i:s))$","")
    case x if x.matches(".*?(?i:bus)(?i:es)$") => replaceAndRespectCase(x,"(.*?(?i:bus))((?i:es))$","")
    case x if x.matches(".*?(?i:sho)(?i:es)$") => replaceAndRespectCase(x,"(.*?(?i:sho))((?i:es))$","e")
    case x if x.matches(".*?(?i:obo)(?i:es)$") => replaceAndRespectCase(x,"(.*?(?i:o))((?i:es))$","e")
    case x if x.matches(".*?(?i:o)(?i:es)$") => replaceAndRespectCase(x,"(.*?(?i:o))((?i:es))$","")
    case x if x.matches(".*?(?i:cris|test)(?i:is|es)$") => replaceAndRespectCase(x,"(.*?(?i:cris|test))((?i:is|es))$","is")
    case x if x.matches(".*?(?i:octop|vir)(?i:us|i)$") => replaceAndRespectCase(x,"(.*?(?i:octop|vir))((?i:us|i))$","us")
    case x if x.matches(".*?(?i:alias|status)(?i:es)$") => replaceAndRespectCase(x,"(.*?(?i:alias|status))((?i:es))$","")
    case x if x.matches(".*?(?i:quiz)(?i:zes)$") => replaceAndRespectCase(x,"(.*?(?i:quiz))((?i:zes))$","")
    case x if x.matches(".*?(?i:ases)$") => replaceAndRespectCase(x,"^(.*(?i:ase))((?i:s))$","")
    case x if x.matches(".*?(?i:[^aeiou]s)$") => replaceAndRespectCase(x,"^(.*(?i:[^aeiou]))((?i:s))$","")
    case x => x
  }
  def replaceAndRespectCase(str: String,pattern: String,rstr:String):String = {
    val suffix = str.replaceAll(pattern,"$2")
    val pcase = defCase(if (suffix!="") suffix else str)
    var rrstr=rstr
    if (pcase=="upper"){
      rrstr=rstr.toUpperCase()
    }else if(pcase=="lower"){
      rrstr=rstr.toLowerCase()
    }
    str.replaceAll(pattern,"$1"+rrstr)
  }
  def defCase(str: String):String = str match {
    case x if x == x.toUpperCase() => "upper"
    case x if x == x.toLowerCase() => "lower"
    case _ => "mixed"
  }
}