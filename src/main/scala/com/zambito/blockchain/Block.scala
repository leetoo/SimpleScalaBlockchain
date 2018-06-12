package com.zambito.blockchain

import scala.compat.Platform
import scala.xml.Elem

case class Block(data: String, previousHash: String, nonce: Int = 0) {
  val timeStamp: Long = Platform.currentTime
  val hash: String = Block.calculateHash(this)
  val isNonceValid: Boolean = hash.startsWith(Array.fill(DIFFICULTY)('0').mkString)

  def toXML: Elem = {
    <block>
      <hash>{hash}</hash>
      <previousHash>{previousHash}</previousHash>
      <data>{data}</data>
      <timeStamp>{timeStamp}</timeStamp>
      <nonce>{nonce}</nonce>
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