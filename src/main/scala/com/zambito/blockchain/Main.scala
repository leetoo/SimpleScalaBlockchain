package com.zambito.blockchain

import org.bouncycastle.jce.provider.BouncyCastleProvider
import scala.xml.PrettyPrinter
import java.security.Security
import scala.collection.mutable


object Main extends App {
  Security.addProvider(new BouncyCastleProvider)

  val coinBase = new Wallet
  val alice = new Wallet
  val bob = new Wallet

  val blockchain = mutable.ListBuffer[Block]()

//  lazy val blockchain: Blockchain =
//    Block(Seq(), "0").mined #::
//      Block(Seq(), blockchain.head.hash).mined #::
//      Block(Seq(), blockchain(1).hash).mined #::
//      Stream.empty[Block]

  val UTXOs = mutable.Map[String, TransactionOutput]()

  val genesisTransaction = Transaction(
    coinBase.publicKey,
    alice.publicKey,
    100f,
    Seq()
  ).signedWith(coinBase.privateKey)

  UTXOs.put(genesisTransaction.outputs.head.id, genesisTransaction.outputs.head)

  val genesis = Block(Seq(genesisTransaction), "0").mined
  blockchain += genesis


  def printBlockchain(blockchain: Blockchain): Unit = {
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

  def processTransaction(transaction: Transaction, block: Block): Block = {
    if(transaction.hasValidSignature && transaction.getInputsValue >= MIN_TRANSACTION) {
      transaction.outputs
        .foreach(o => UTXOs.put(o.id, o))

      transaction.inputs
        .map(i => i.copy(UTXO = UTXOs.get(i.transactionOutputId)))
        .flatMap(_.UTXO)
        .foreach(o => UTXOs.remove(o.id))

      block.copy(transactions = transaction +: block.transactions)
    } else block
  }


  printBlockchain(blockchain)

  println(s"Is valid: ${isChainValid(blockchain)}")

  println(s"Alice wallet size: ${alice.getBalance(UTXOs)}")
}
