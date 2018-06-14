package com.zambito

import java.security._
import java.util.Base64
import com.zambito.blockchain._

/**
  * Simple blockchain built using Scala
  *
  * This blockchain features:
  *
  *   1. Proof-of-work block mining.
  *   1. [[Block]]s which represent a set of valid transactions that occurred since the last block was mined.
  *   1. [[Wallet]]s which can create transactions within the bounds of their funds.
  *   1. A [[Blockchain]] which can be validated by checking the hashes of the blocks to make sure no changes have
  *      occurred, and checking the validity of the transactions on each block.
  *   1. A collection of UTXOs (unspent transaction outputs) to quickly check the funds of a wallet, and for various
  *      other operations.
  *
  * All operations are done in a functional style. There are is no use of the keyword `var` in this project. This
  * implementation would not be good for a long term solution, because append operations on immutable collections are
  * highly inefficient when they grow to a large size. A more reasonable solution would choose to use a mutable
  * collection to represent the blockchain, where race conditions are a non-issue due to the nature of the
  * tamper-resistance. This project was created purely for practice reasons.
  *
  * Built following along with [[https://medium.com/programmers-blockchain/create-simple-blockchain-java-tutorial-from-scratch-6eeed3cb03fa this tutorial]].
  *
  * Found from in [[https://github.com/openblockchains/awesome-blockchains this repo]].
  *
  * @author Robby Zambito
  * @see [[https://github.com/Zambito1/SimpleScalaBlockchain/blob/master/src/main/scala/com/zambito/blockchain/package.scala `Source`]]
  */
package object blockchain {
  /** Represents the number of leading 0s required for a valid block hash */
  val DIFFICULTY = 5

  /** Minimum value of a valid transaction */
  val MIN_TRANSACTION = 0.0f

  /** Implicit functions which extend strings. Allows for strings to encrypt themselves. */
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

  /** Allows Keys to be printed in hex. */
  implicit class KeyUtil(key: Key) {
    def getStringFromKey: String =
      Base64.getEncoder.encodeToString(key.getEncoded)
  }

  /** Allows third parties to verify the signature on some data given a publicKey */
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
