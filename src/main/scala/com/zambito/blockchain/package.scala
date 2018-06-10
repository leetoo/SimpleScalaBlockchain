package com.zambito

package object blockchain {
  implicit class StringUtil(str: String) {
    import java.security.MessageDigest
    import java.math.BigInteger

    def encrypt: String =
      String.format(
        "%032x",
        new BigInteger(
          1,
          MessageDigest.getInstance("SHA-256").digest(str.getBytes("UTF-8"))
        )
      )
  }
}
