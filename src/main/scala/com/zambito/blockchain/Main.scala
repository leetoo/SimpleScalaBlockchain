package com.zambito.blockchain

import scala.xml.PrettyPrinter


object Main extends App {

  lazy val blockchain: Blockchain =
    Block("First block data", "0") #::
    Block("This is the second block", blockchain.head.hash) #::
    Block("Data for the third block", blockchain(1).hash) #::
    Stream.empty[Block]

  println(new PrettyPrinter(195, 4).format(
    <blockchain>{ blockchain.map(_.toXML) }</blockchain>
  ))

  def isChainValid: Blockchain => Boolean = {
    case fst +: snd +: tail
      if snd.hash == Block.calculateHash(snd) &&
        fst.hash == snd.previousHash => isChainValid(snd +: tail)
    case fst +: snd +: _
      if snd.hash != Block.calculateHash(snd) ||
      fst.hash != snd.previousHash => false
    case _ => true
  }

  println(s"Is valid: ${isChainValid(blockchain)}")
}
