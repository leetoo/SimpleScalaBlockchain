package com.zambito.blockchain

case class Blockchain(blocks: Seq[Block] = Seq(),
                      UTXOs: Map[String, TransactionOutput]) {

  def isValidChain(blocks: Seq[Block] = blocks): Boolean = {

    def checkHashes: Seq[Block] => Boolean = {
      case fst +: snd +: tail
        if snd.hash == snd.hash &&
          fst.hash == snd.previousHash &&
          snd.isNonceValid => checkHashes(snd +: tail)

      case fst +: snd +: _
        if snd.hash != snd.hash ||
          fst.hash != snd.previousHash ||
          !snd.isNonceValid => false
      case _ => true
    }

    checkHashes(blocks)
  }

  def processTransaction(transaction: Transaction, block: Block): (Blockchain, Block) = {
    if(transaction.hasValidSignature && transaction.getInputsValue >= MIN_TRANSACTION) {

      this.copy(UTXOs = UTXOs ++
        transaction.outputs.map(o => o.id -> o) --
        transaction.inputs.map(_.transactionOutputId)) ->
        block.copy(transactions = transaction +: block.transactions)
    } else (this, block)
  }

}
