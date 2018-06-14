package com.zambito.blockchain

import java.security.Security
import org.bouncycastle.jce.provider.BouncyCastleProvider
import scala.xml.PrettyPrinter


object Main extends App {
  Security.addProvider(new BouncyCastleProvider)

  val coinBase = new Wallet
  val alice = new Wallet
  val bob = new Wallet


  val genesisTransaction = Transaction(
    coinBase.publicKey,
    alice.publicKey,
    100f,
    Seq()
  ).signedWith(coinBase.privateKey)

  val genesis = Block(Seq(genesisTransaction), "0").mined

  val blockchain = Blockchain(
    Seq(genesis),
    Map(genesisTransaction.outputs.head.id -> genesisTransaction.outputs.head)
  )

  def printBlockchain(blockchain: Blockchain): Unit = {
    println("<blockchain>")
    blockchain.blocks.map(b =>
      new PrettyPrinter(195, 4).format(b.toXML)
        .replaceAll("^", "    ")
        .replaceAll("\n", "\n    "))
      .foreach(println)
    println("</blockchain>")
  }



  printBlockchain(blockchain)

  println(s"Is valid: ${blockchain.isValidChain()}")

  println(s"Alice balance: ${alice.getBalance(blockchain.UTXOs)}")

  println(s"Bobs balance: ${bob.getBalance(blockchain.UTXOs)}")
}
