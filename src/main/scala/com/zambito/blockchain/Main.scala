package com.zambito.blockchain

import scala.xml.PrettyPrinter

object Main extends App {

  lazy val blockchain: Stream[Block] =
    Block("First block data", "0") #::
    Block("This is the second block", blockchain.head.hash) #::
    Block("Data for the third block", blockchain(1).hash) #::
    Stream.empty[Block]

  println(
    new PrettyPrinter(195, 4).format(
      <blockchain>{ blockchain.map(_.toXML) }</blockchain>
    )
  )
}
