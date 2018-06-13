package com.zambito.blockchain

import java.security._
import java.security.spec.ECGenParameterSpec
import scala.annotation.tailrec
import scala.collection.Map

class Wallet {
  val (privateKey, publicKey) = {
    val keyGen = KeyPairGenerator.getInstance("ECDSA", "BC")
    val random = SecureRandom.getInstance("SHA1PRNG")
    val ecSpec = new ECGenParameterSpec("prime192v1")
    keyGen.initialize(ecSpec, random)

    val keyPair = keyGen.generateKeyPair

    (keyPair.getPrivate, keyPair.getPublic)
  }

  def getBalance(UTXOs: Map[String, TransactionOutput]): Float = {
    UTXOs.values
      .filter(_.isMine(publicKey))
      .map(_.value)
      .sum
  }

  def sendFunds(recipient: PublicKey, value: Float, UTXOs: Map[String, TransactionOutput]): Option[Transaction] = {
    if(getBalance(UTXOs) >= value) {

      @tailrec
      def aux(utxos: Map[String, TransactionOutput],
              inputs: Seq[TransactionInput] = Seq(),
              total: Float = 0.0f): Seq[TransactionInput] = {
        if(total >= value) {
          inputs
        } else utxos.head match {
          case (id, o) =>
            aux(utxos.tail, TransactionInput(id, Some(o)) +: inputs, total + o.value)
        }
      }

      val inputs = aux(UTXOs)

      Some {
        Transaction(publicKey, recipient, value, inputs).signedWith(privateKey)
      }
    } else None
  }

}
