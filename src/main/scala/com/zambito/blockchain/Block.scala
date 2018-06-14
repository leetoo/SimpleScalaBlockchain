package com.zambito.blockchain

import scala.compat.Platform
import scala.xml.Elem

/**
  * A block represents a [[Seq]] of [[Transaction]]s
  * @param transactions Transactions stored in the block
  * @param previousHash Represents the [[Block]] that this [[Block]] extends.
  * @param timeStamp Time that was defined
  * @param nonce Random number that must be found to validate the hash of this [[Block]]
  *
  * @author Robby Zambito
  * @see [[https://github.com/CryptoKass/NoobChain-Tutorial-Part-2/blob/master/src/noobchain/Block.java `Block.java`]]
  */
case class Block(transactions: Seq[Transaction],
                 previousHash: String,
                 timeStamp: Long = Platform.currentTime,
                 nonce: Int = 0) {

  /** A hash value which represents all of the transactions included in this block. */
  val merkleRoot: String = {
    def aux: Seq[String] => String = {
      case fst +: snd +: tail => aux((fst + snd).encrypted() +: tail)
      case head +: Seq() => head
      case _ => ""
    }

    aux(transactions.map(_.transactionId))
  }

  /** Hash value which represents the block itself. */
  val hash: String = (previousHash + timeStamp.toString + nonce.toString + merkleRoot).encrypted()

  /** True if the block has been mined, false otherwise */
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

  /**
    * Tries nonce values starting from 0 until the value is found which will make the hash of the block start with a
    * certain number of 0s.
    *
    * @example
    *          In this block, a nonce of 12073 was found which will result in a hash beginning with four 0s
    *
    *     <block>
    *         <hash>0000e35e7d9f690012cb46c716d355163a7a4dc0becb58d6b1cd2081143b39e0</hash>
    *         <previousHash>0</previousHash>
    *         <merkleRoot>7ca48ef1d0c89fae7c3c1711b0fec2a165f5b675366f67901b4036e65ed47f4f</merkleRoot>
    *         <timeStamp>1528953534175</timeStamp>
    *         <nonce>12073</nonce>
    *     </block>
    * @return
    */
  def mined: Block = {
    Stream.from(0)
      .map(n => this.copy(timeStamp = Platform.currentTime, nonce = n))
      .filter(_.isNonceValid)
      .head
  }

}