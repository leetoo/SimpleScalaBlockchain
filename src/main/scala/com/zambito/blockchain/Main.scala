package com.zambito.blockchain

import scala.xml.PrettyPrinter


object Main extends App {

  lazy val blockchain: Blockchain =
    Block.mineBlock(Block("First block data", "0")) #::
    Block.mineBlock(Block("This is the second block", blockchain.head.hash)) #::
    Block.mineBlock(Block("Data for the third block", blockchain(1).hash)) #::
    Stream.empty[Block]

  println(new PrettyPrinter(195, 4).format(
    <blockchain>{ blockchain.map(_.toXML) }</blockchain>
  ))

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

  println(s"Is valid: ${isChainValid(blockchain)}")
}
