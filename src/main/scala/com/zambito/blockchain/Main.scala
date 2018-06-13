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
    Block.mineBlock(Block("First block data", "0")) #::
    Block.mineBlock(Block("This is the second block", blockchain.head.hash)) #::
    Block.mineBlock(Block("Data for the third block", blockchain(1).hash)) #::
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
      if snd.hash == Block.calculateHash(snd) &&
         fst.hash == snd.previousHash &&
         snd.isNonceValid => isChainValid(snd +: tail)

    case fst +: snd +: _
      if snd.hash != Block.calculateHash(snd) ||
         fst.hash != snd.previousHash ||
         !snd.isNonceValid => false

    case _ => true
  }

  def processTransaction(transaction: Transaction): Boolean = {
    if(transaction.hasValidSignature && transaction.getInputsValue >= MIN_TRANSACTION) {
      transaction.outputs
        .foreach(o => UTXOs.put(o.id, o))

      transaction.inputs
        .map(ti => ti.copy(UTXO = UTXOs.get(ti.transactionOutputId)))
        .flatMap(_.UTXO)
        .foreach(o => UTXOs.remove(o.id))
      true
    } else false
  }


  val unsignedTransaction = Transaction(bob.publicKey, alice.publicKey, 5, Seq[TransactionInput]())

  val transaction = Transaction.signTransaction(
    unsignedTransaction,
    bob.privateKey
  )


  println(s"Valid signature on unsigned: ${unsignedTransaction.hasValidSignature}")
  println(s"Valid signature: ${transaction.hasValidSignature}")
  println(s"Transaction: $transaction")


  lazyBlockchainPrint(blockchain)

  println(s"Is valid: ${isChainValid(blockchain)}")
}
