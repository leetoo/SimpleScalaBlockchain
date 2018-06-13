package com.zambito

import java.security._
import java.util.Base64

package object blockchain {
  val DIFFICULTY = 3

  type Blockchain = Seq[Block]

  implicit class StringUtil(str: String) {

    def encrypted(algorithm: String = "SHA-256"): String = {
      val digest = MessageDigest.getInstance(algorithm)
      val hash = digest.digest(str.getBytes("UTF-8"))

      hash.map(byte => Integer.toHexString(0xff & byte))
        .map(s => if(s.length == 1) s + "0" else s)
        .mkString
    }

    def signData(privateKey: PrivateKey): Array[Byte] = {
      val dsa = Signature.getInstance("ECDSA", "BC")
      dsa.initSign(privateKey)
      dsa.update(str.getBytes)
      dsa.sign()
    }
  }

  implicit class KeyUtil(key: Key) {
    def getStringFromKey: String =
      Base64.getEncoder.encodeToString(key.getEncoded)
  }

  implicit class PublicKeyUtil(pk: PublicKey) {
    def verifySig(data: String, signature: Array[Byte]): Boolean = {
      try {
        val ecdsaVerify = Signature.getInstance("ECDSA", "BC")
        ecdsaVerify.initVerify(pk)
        ecdsaVerify.update(data.getBytes)
        ecdsaVerify.verify(signature)
      } catch {
        case _: Exception => false
      }
    }
  }

}
