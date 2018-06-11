package com.zambito.blockchain

import com.google.gson.GsonBuilder
import scala.collection.JavaConverters

object Main extends App {

  lazy val blockchain: Stream[Block] =
    Block("First block data", "0") #::
      Block("This is the second block", blockchain(0).hash) #::
      Block("Data for the third block", blockchain(1).hash) #::
      Stream.empty[Block]

  println(
    new GsonBuilder().setPrettyPrinting().create().toJson(JavaConverters.seqAsJavaList(blockchain))
  )
}
