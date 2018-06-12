package com.zambito.blockchain

import scala.compat.Platform
import scala.xml.Elem

case class Block(data: String, previousHash: String, nonce: Int = 0) {
  val timeStamp: Long = Platform.currentTime
  val hash: String = Block.calculateHash(this)
  val isNonceValid: Boolean = hash.startsWith(Array.fill(DIFFICULTY)('1').mkString)

  def toXML: Elem = {
    <block>
      <data>{data}</data>
      <previousHash>{previousHash}</previousHash>
      <nonce>{nonce}</nonce>
      <timeStamp>{timeStamp}</timeStamp>
      <hash>{hash}</hash>
    </block>
  }
}

object Block {
  def calculateHash(b: Block): String =
    (b.previousHash + b.timeStamp.toString + b.nonce.toString +  b.data).encrypted()

  def mineBlock(block: Block): Block = {
      Stream.from(0)
        .map(n => block.copy(nonce = n))
        .filter(_.isNonceValid)
        .head
  }

}