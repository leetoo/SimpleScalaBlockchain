import scala.compat.Platform

case class Block(data: String, previousHash: String) {
  val timeStamp: Long = Platform.currentTime


}
