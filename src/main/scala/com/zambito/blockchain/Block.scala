package com.zambito.blockchain

import scala.compat.Platform

case class Block(data: String, previousHash: String) {
  val timeStamp: Long = Platform.currentTime
  val hash: String = (previousHash + timeStamp.toString + data).encrypted()
}
