package com.mx.par

import java.nio.ByteBuffer
import java.util.concurrent.Executors

import com.lmax.disruptor.dsl.Disruptor
import com.lmax.disruptor.{RingBuffer, EventHandler, EventFactory}

/**
 * Created by milo on 15-7-30.
 */
object DisruptorDemo {

  class LongEvent {
    var value: Long = _
    def set(v: Long) { value = v }
  }

  class LongEventFactory extends EventFactory[LongEvent] {
    def newInstance(): LongEvent = {
      new LongEvent()
    }
  }

  class LongEventHandler extends EventHandler[LongEvent] {
    def onEvent(event: LongEvent, sequence: Long, endOfBatch: Boolean) {
      println("Event: " + event)
    }
  }

  class LongEventProducer(val ringBuffer: RingBuffer[LongEvent]) {
    def onData(bb: ByteBuffer) = {
      val sequence = ringBuffer.next()
      try {
        val event = ringBuffer.get(sequence)
        event.set(bb.getLong(0))
      } finally {
        ringBuffer.publish(sequence)
      }
    }
  }

  def main(args: Array[String]) {
    val executor = Executors.newCachedThreadPool()
    val factory = new LongEventFactory()
    val bufferSize = 1024
    val disruptor = new Disruptor[LongEvent](factory, bufferSize, executor)
    disruptor.handleEventsWith(new LongEventHandler())
    disruptor.start()
    val ringBuffer = disruptor.getRingBuffer()
    val producer = new LongEventProducer(ringBuffer)
    val bb = ByteBuffer.allocate(8)
    for (l <- 1 to 1000) {
      bb.putLong(0, l)
      producer.onData(bb)
      Thread.sleep(1000)
    }
  }
}
