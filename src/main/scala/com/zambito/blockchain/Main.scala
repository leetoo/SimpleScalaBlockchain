package com.zambito.blockchain

import scala.xml.PrettyPrinter

object Main extends App {

  lazy val blockchain: Stream[Block] =
    Block("First block data", "0") #::
    Block("This is the second block", blockchain.head.hash) #::
    Block("Data for the third block", blockchain(1).hash) #::
    Stream[Block]()

  def blockToXML(block: Block) = {
    <block>
      <data>{block.data}</data>
      <previousHash>{block.previousHash}</previousHash>
      <timeStamp>{block.timeStamp}</timeStamp>
      <hash>{block.hash}</hash>
    </block>
  }


  println(
    new PrettyPrinter(195, 4).format(
      <blockchain>{ blockchain map blockToXML }</blockchain>
    )
  )
}
