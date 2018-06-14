package com.zambito.blockchain

import java.security._
import java.util.concurrent.atomic.AtomicInteger

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
    TransactionOutput(sender, getInputsValue - value, transactionId)
  )

  val hasValidSignature: Boolean = sender.verifySig(
    sender.getStringFromKey + recipient.getStringFromKey + value.toString,
    signature)

  def getInputsValue: Float = inputs.flatMap(_.UTXO).map(_.value).sum

  def getOutputsValue: Float = outputs.map(_.value).sum

  def signedWith(privateKey: PrivateKey): Transaction = {
    this.copy(signature =
      (sender.getStringFromKey +
        recipient.getStringFromKey +
        value.toString).signData(privateKey))
  }

}

object Transaction {
  private val number = new AtomicInteger(0)
}

case class TransactionInput(transactionOutputId: String,
                            UTXO: Option[TransactionOutput] = None)


case class TransactionOutput(recipient: PublicKey,
                             value: Float,
                             parentTransactionId: String) {
  val id: String = (recipient.getStringFromKey + value.toString + parentTransactionId).encrypted()

  def isMine(publicKey: PublicKey): Boolean =
    publicKey == recipient

}