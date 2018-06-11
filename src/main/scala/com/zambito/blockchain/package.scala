package com.zambito

package object blockchain {
  implicit class StringUtil(str: String) {
    import java.security.MessageDigest
    import java.math.BigInteger

    def encrypted(algorithm: String = "SHA-256"): String =
      String.format(
        "%032x",
        new BigInteger(
          1,
          MessageDigest.getInstance(algorithm).digest(str.getBytes("UTF-8"))
        )
      )
  }
}
