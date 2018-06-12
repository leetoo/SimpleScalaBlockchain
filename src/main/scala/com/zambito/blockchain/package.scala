package com.zambito

package object blockchain {
  val DIFFICULTY = 5

  implicit class StringUtil(str: String) {
    import java.security.MessageDigest

    def encrypted(algorithm: String = "SHA-256"): String = {
      val digest = MessageDigest.getInstance(algorithm)
      val hash = digest.digest(str.getBytes("UTF-8"))

      hash.map(byte => Integer.toHexString(0xff & byte))
        .map(s => if(s.length == 1) s + "0" else s)
        .mkString
    }
  }

  type Blockchain = Seq[Block]

}
