package com.zambito.blockchain

import java.security._

case class Transaction(sender: PublicKey,
                       recipient: PublicKey,
                       value: Float,
                       inputs: Seq[TransactionInput],
                       signature: Array[Byte] = Array[Byte]()) {
  val transactionID: String = {
    Transaction.number += 1

    (sender.getStringFromKey +
      recipient.getStringFromKey +
      value.toString +
      Transaction.number).encrypted()
  }

  val outputs: Seq[TransactionOutput] = Seq[TransactionOutput]()

  val hasValidSignature: Boolean = sender.verifySig(
    sender.getStringFromKey + recipient.getStringFromKey + value.toString,
    signature)
}

object Transaction {
  private var number = 0

  def signTransaction(transaction: Transaction, privateKey: PrivateKey): Transaction = {
    transaction.copy(signature =
      (transaction.sender.getStringFromKey +
      transaction.recipient.getStringFromKey +
      transaction.value.toString).signData(privateKey))
  }
}

class TransactionInput
class TransactionOutput