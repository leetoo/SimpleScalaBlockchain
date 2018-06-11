package com.zambito.blockchain

import scala.compat.Platform
import scala.xml.Elem

case class Block(data: String, previousHash: String) {
  val timeStamp: Long = Platform.currentTime
  val hash: String = Block.calculateHash(this)

  def toXML: Elem = {
    <block>
      <data>{data}</data>
      <previousHash>{previousHash}</previousHash>
      <timeStamp>{timeStamp}</timeStamp>
      <hash>{hash}</hash>
    </block>
  }
}

object Block {
  def calculateHash(b: Block): String = {
    (b.previousHash + b.timeStamp.toString + b.data).encrypted()
  }
}