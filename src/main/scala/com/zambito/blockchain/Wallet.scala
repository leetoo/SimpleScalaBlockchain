package com.zambito.blockchain

import java.security._
import java.security.spec.ECGenParameterSpec
import scala.annotation.tailrec
import scala.collection.Map

/**
  * A wallet represents an entity which can own funds.
  *
  * @author Robby Zambito
  * @see [[https://github.com/CryptoKass/NoobChain-Tutorial-Part-2/blob/master/src/noobchain/Wallet.java `Wallet.java`]]
  */
class Wallet {
  val (privateKey, publicKey) = {
    val keyGen = KeyPairGenerator.getInstance("ECDSA", "BC")
    val random = SecureRandom.getInstance("SHA1PRNG")
    val ecSpec = new ECGenParameterSpec("prime192v1")
    keyGen.initialize(ecSpec, random)

    val keyPair = keyGen.generateKeyPair

    (keyPair.getPrivate, keyPair.getPublic)
  }

  /**
    * Finds the sum of the UTXOs in the given blockchain that belong to the current wallet.
    * @param blockchain Context that the wallet may own a balance in.
    * @return Balance of the wallet in the blockchain.
    */
  def getBalance(blockchain: Blockchain): Float =
    blockchain.UTXOs.values.filter(_.isMine(publicKey)).map(_.value).sum

  /**
    * If possible, creates a transaction to send funds to some other wallet.
    * @param recipient `PublicKey` of the wallet to send funds to.
    * @param value Amount of currency to be sent to the recipient and removed from the current wallet.
    * @param blockchain Context for the transaction to take place in.
    * @return `Some Transaction` if current wallet has the funds to complete the transaction.
    *         `None` if the current wallet does not have the funds to complete the transaction.
    */
  def sendFunds(recipient: PublicKey, value: Float, blockchain: Blockchain): Option[Transaction] = {
    if(getBalance(blockchain) >= value) {

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

      val inputs = aux(blockchain.UTXOs)

      Some {
        Transaction(publicKey, recipient, value, inputs).signedWith(privateKey)
      }
    } else None
  }

}
