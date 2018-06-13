package com.zambito.blockchain

import org.bouncycastle.jce.provider.BouncyCastleProvider
import scala.xml.PrettyPrinter
import java.security.Security
import scala.collection.mutable


object Main extends App {
  Security.addProvider(new BouncyCastleProvider)

  val alice = new Wallet
  val bob = new Wallet

  lazy val blockchain: Blockchain =
    Block(Seq(), "0").mined #::
    Block(Seq(), blockchain.head.hash).mined #::
    Block(Seq(), blockchain(1).hash).mined #::
    Stream.empty[Block]

  val UTXOs: mutable.Map[String, TransactionOutput] = mutable.Map()


  def lazyBlockchainPrint(blockchain: Blockchain): Unit = {
    println("<blockchain>")
    blockchain.map(b =>
      new PrettyPrinter(195, 4).format(b.toXML)
        .replaceAll("^", "    ")
        .replaceAll("\n", "\n    "))
      .foreach(println)
    println("</blockchain>")
  }

  def isChainValid: Blockchain => Boolean = {
    case fst +: snd +: tail
      if snd.hash == snd.hash &&
         fst.hash == snd.previousHash &&
         snd.isNonceValid => isChainValid(snd +: tail)

    case fst +: snd +: _
      if snd.hash != snd.hash ||
         fst.hash != snd.previousHash ||
         !snd.isNonceValid => false

    case _ => true
  }

  def processTransaction(transaction: Transaction): Boolean = {
    if(transaction.hasValidSignature && transaction.getInputsValue >= MIN_TRANSACTION) {
      transaction.outputs
        .foreach(o => UTXOs.put(o.id, o))

      transaction.inputs
        .map(i => i.copy(UTXO = UTXOs.get(i.transactionOutputId)))
        .flatMap(_.UTXO)
        .foreach(o => UTXOs.remove(o.id))
      true
    } else false
  }


  val unsignedTransaction = Transaction(bob.publicKey, alice.publicKey, 5, Seq[TransactionInput]())

  val transaction = unsignedTransaction.signedWith(bob.privateKey)


  println(s"Valid signature on unsigned: ${unsignedTransaction.hasValidSignature}")
  println(s"Valid signature: ${transaction.hasValidSignature}")
  println(s"Transaction: $transaction")


  lazyBlockchainPrint(blockchain)

  println(s"Is valid: ${isChainValid(blockchain)}")
}
