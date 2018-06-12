package com.zambito.blockchain

import java.security._
import java.security.spec.ECGenParameterSpec

class Wallet {
  val (privateKey, publicKey) = {
    val keyGen = KeyPairGenerator.getInstance("ECDSA", "BC")
    val random = SecureRandom.getInstance("SHA1PRNG")
    val ecSpec = new ECGenParameterSpec("prime192v1")
    keyGen.initialize(ecSpec, random)

    val keyPair = keyGen.generateKeyPair()

    (keyPair.getPrivate, keyPair.getPublic)
  }


}
