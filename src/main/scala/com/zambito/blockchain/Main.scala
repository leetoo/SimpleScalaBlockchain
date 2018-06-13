package com.zambito.blockchain

import org.bouncycastle.jce.provider.BouncyCastleProvider
import scala.xml.PrettyPrinter
import java.security.Security


object Main extends App {
  Security.addProvider(new BouncyCastleProvider)

  val bob = new Wallet
  val alice = new Wallet

  lazy val blockchain: Blockchain =
    Block.mineBlock(Block("First block data", "0")) #::
    Block.mineBlock(Block("This is the second block", blockchain.head.hash)) #::
    Block.mineBlock(Block("Data for the third block", blockchain(1).hash)) #::
    Stream.empty[Block]


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


  println(s"Bob's private key: ${bob.privateKey}")
  println(s"Bob's public key:  ${bob.publicKey}")


  val unsignedTransaction = Transaction(bob.publicKey, aklice.publicKey, 5, Seq[TransactionInput]())

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
