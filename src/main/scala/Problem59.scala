/*
Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange). For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value, taken from a secret key. The advantage with the XOR function is that using the same encryption key on the cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes. The user would keep the encrypted message and the encryption key in different locations, and without both "halves", it is impossible to decrypt the message.

Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key. If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message. The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.

Your task has been made easy, as the encryption key consists of three lower case characters. Using cipher1.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes, and the knowledge that the plain text must contain common English words, decrypt the message and find the sum of the ASCII values in the original text.
*/

object Problem59 extends App{

  type Letter = Int
  type Msg = Array[Letter]
  type Passw = Array[Set[Letter]]
  
  val passwChars = ('a' to 'z').map(_.toInt).toSet 
  
  def findPassw( msg: Msg, passwSize : Int = 3 ) : Passw = {
  
    def isPossibleInMsg( c: Int ) = c.toChar.isLetter || c.toChar.isWhitespace
  
    val candidates : Array[Set[Letter]] = Array.tabulate(msg.size){ i =>
      passwChars.filter( c => isPossibleInMsg(c ^ msg(i)) ).toSet
    }
    
    for( i <- 0 until msg.size ){
      println( s"Candidates for ${msg(i)} => ${candidates(i)}  ${candidates(i).map(_.toChar)}" )
      candidates(i).foreach( c=> print( s" ${(msg(i)^c)}->${(msg(i)^c).toChar}" ) )
      println()
    }
    
    val passw = for( i <- 0 until passwSize ) yield {
      val chars : Iterator[Set[Letter]] = candidates.drop(i).sliding(1,passwSize).map( _(0) )
      chars.reduce( _ & _ )
    }
    
    passw.toArray
  }
  
  def decode( msg: Msg, passw : String ) = {
    val ret = for( i <- 0 until msg.size ) yield{
      msg(i) ^ passw(i%passw.size)
    }
    ret.toArray.mkString
  }
  
  val values = scala.io.Source.fromFile( "./src/main/scala/cipher1.txt" ).
               getLines.
               flatMap( l => l.split(",").map( _.toInt ) ).
               toArray
               
  println( s"values: ${values.mkString(",")}" )
  
  try{
    val passw = findPassw(values).map( _.mkString ).mkString
    
    println( s"passw: $passw" )
    
    val decoded = decode( values, passw )
    println( s"decoded: $decoded" )
  }
  catch{
    case t: Throwable => 
      println( s"Error: $t" )
      t.printStackTrace(System.out)
  }
}