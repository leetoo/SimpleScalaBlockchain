package com.zambito.blockchain

import scala.compat.Platform
import scala.xml.Elem

case class Block(data: String, previousHash: String) {
  val timeStamp: Long = Platform.currentTime
  val hash: String = (previousHash + timeStamp.toString + data).encrypted()

  def toXML: Elem = {
    <block>
      <data>{data}</data>
      <previousHash>{previousHash}</previousHash>
      <timeStamp>{timeStamp}</timeStamp>
      <hash>{hash}</hash>
    </block>
  }
}
