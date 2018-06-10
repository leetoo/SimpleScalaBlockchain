package com.zambito.blockchain

object RunnableBlockChain extends App {
  val genesis = Block("First block data", "0")

  println(s"First block: $genesis")
  println(s"First block hash: ${genesis.hash}")

  val secondBlock = Block("This is the second block", genesis.hash)

  println(s"First block: $secondBlock")
  println(s"First block hash: ${secondBlock.hash}")

  val thirdBlock = Block("Data for the third block", secondBlock.hash)

  println(s"First block: $thirdBlock")
  println(s"First block hash: ${thirdBlock.hash}")

}
