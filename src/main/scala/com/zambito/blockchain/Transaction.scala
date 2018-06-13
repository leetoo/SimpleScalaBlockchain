package com.zambito.blockchain

import java.security._
import java.util.concurrent.atomic.AtomicInteger

case class Transaction(sender: PublicKey,
                       recipient: PublicKey,
                       value: Float,
                       inputs: Seq[TransactionInput],
                       signature: Array[Byte] = Array[Byte]()) {
  val transactionId: String = {
    Transaction.number.incrementAndGet()

    (sender.getStringFromKey +
      recipient.getStringFromKey +
      value.toString +
      Transaction.number).encrypted()
  }

  val outputs: Seq[TransactionOutput] = Seq(
    TransactionOutput(recipient, value, transactionId),
    TransactionOutput(sender, getInputsValue - value, transactionId)
  )

  val hasValidSignature: Boolean = sender.verifySig(
    sender.getStringFromKey + recipient.getStringFromKey + value.toString,
    signature)

  def getInputsValue: Float = inputs.flatMap(_.UTXO).map(_.value).sum

  def getOutputValue: Float = outputs.map(_.value).sum

}

object Transaction {
  private val number = new AtomicInteger(0)

  def signTransaction(transaction: Transaction, privateKey: PrivateKey): Transaction = {
    transaction.copy(signature =
      (transaction.sender.getStringFromKey +
      transaction.recipient.getStringFromKey +
      transaction.value.toString).signData(privateKey))
  }
}

case class TransactionInput(transactionOutputId: String,
                            UTXO: Option[TransactionOutput] = None)


case class TransactionOutput(recipient: PublicKey,
                             value: Float,
                             parentTransactionId: String) {
  val id: String = (recipient.getStringFromKey + value.toString + parentTransactionId).encrypted()

  def isMine(publicKey: PublicKey): Boolean = publicKey == recipient

}