package com.zambito.blockchain

import scala.compat.Platform
import scala.xml.Elem

case class Block(transactions: Seq[Transaction],
                 previousHash: String,
                 timeStamp: Long = Platform.currentTime,
                 nonce: Int = 0) {
  val merkleRoot: String = {
    def aux: Seq[String] => String = {
      case fst +: snd +: tail => aux((fst + snd).encrypted() +: tail)
      case head +: Seq() => head
      case _ => ""
    }

    aux(transactions.map(_.transactionId))
  }

  val hash: String = (previousHash + timeStamp.toString + nonce.toString + merkleRoot).encrypted()
  val isNonceValid: Boolean = hash.startsWith(Array.fill(DIFFICULTY)('0').mkString)

  def toXML: Elem = {
    <block>
      <hash>{hash}</hash>
      <previousHash>{previousHash}</previousHash>
      <merkleRoot>{merkleRoot}</merkleRoot>
      <timeStamp>{timeStamp}</timeStamp>
      <nonce>{nonce}</nonce>
    </block>
  }

  def mined: Block = {
    Stream.from(0)
      .map(n => this.copy(timeStamp = Platform.currentTime, nonce = n))
      .filter(_.isNonceValid)
      .head
  }

}