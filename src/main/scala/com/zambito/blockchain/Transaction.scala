package com.zambito.blockchain

import java.security._
import java.util.concurrent.atomic.AtomicInteger

/**
  * A transaction of currency from one [[Wallet]] to another.
  *
  * @param sender `PublicKey` of the [[Wallet]] that initiated the transaction, depleting their funds.
  * @param recipient `PublicKey` of the [[Wallet]] which receives the funds.
  * @param value To be depleted from the senders balance and added to the recipients balance.
  * @param inputs Proof the sender recieved to funds at some point.
  * @param signature Approval of the sender.
  * @author Robby Zambito
  * @see [[https://github.com/CryptoKass/NoobChain-Tutorial-Part-2/blob/master/src/noobchain/Transaction.java `Transaction.java`]]
  */
case class Transaction(sender: PublicKey,
                       recipient: PublicKey,
                       value: Float,
                       inputs: Seq[TransactionInput],
                       signature: Array[Byte] = Array[Byte]()) {
  val transactionId: String = {
    (sender.getStringFromKey +
      recipient.getStringFromKey +
      value.toString +
      Transaction.number.incrementAndGet().toString).encrypted()
  }

  val outputs: Seq[TransactionOutput] = Seq(
    TransactionOutput(recipient, value, transactionId),
    TransactionOutput(sender, getInputsValue - value, transactionId))

  val hasValidSignature: Boolean = sender.verifySig(
    sender.getStringFromKey + recipient.getStringFromKey + value.toString,
    signature)

  def getInputsValue: Float =
    inputs.flatMap(_.UTXO).map(_.value).sum

  def getOutputsValue: Float =
    outputs.map(_.value).sum

  /**
    * Approves the transaction with the signature of the sender.
    * @param privateKey `PrivateKey` of the sender.
    * @return Transaction which has been approved by the sender.
    */
  def signedWith(privateKey: PrivateKey): Transaction = {
    this.copy(signature =
      (sender.getStringFromKey +
        recipient.getStringFromKey +
        value.toString).signData(privateKey))
  }

}

/** Simply used to avoid hash collisions between separate [[Transaction]]s */
object Transaction {
  private val number = new AtomicInteger(0)
}

/**
  * Represents the previous transaction which the sender recieved the funds to create a transaction.
  * @param transactionOutputId Hash of the [[TransactionOutput]] that put the sender in control of the funds.
  * @param UTXO The actual [[TransactionOutput]]. If None, the [[TransactionInput]] has no real value.
  * @see [[https://github.com/CryptoKass/NoobChain-Tutorial-Part-2/blob/master/src/noobchain/TransactionInput.java `TransactionInput.java`]]
  */
case class TransactionInput(transactionOutputId: String,
                            UTXO: Option[TransactionOutput] = None)


/**
  * Represents the output of a [[Transaction]]. This is referenced in the future when the current recipient would like
  * to create their own [[Transaction]] with the funds they recieved from this [[Transaction]].
  * @param recipient `PublicKey` of the [[Wallet]] which receives the funds.
  * @param value To be added to the recipients balance.
  * @param parentTransactionId ID of the transaction which created this output.
  * @see [[https://github.com/CryptoKass/NoobChain-Tutorial-Part-2/blob/master/src/noobchain/TransactionOutput.java `TransactionOutput.java`]]
  */
case class TransactionOutput(recipient: PublicKey,
                             value: Float,
                             parentTransactionId: String) {
  val id: String = (recipient.getStringFromKey + value.toString + parentTransactionId).encrypted()

  def isMine(publicKey: PublicKey): Boolean =
    publicKey == recipient

}