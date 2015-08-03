package com.mx.par

import java.nio.ByteBuffer
import java.util.concurrent.Executors

import com.lmax.disruptor.dsl.Disruptor
import com.lmax.disruptor.{RingBuffer, EventHandler, EventFactory}
import com.mx.par.day06.{Get, GetUp, Request}

/**
 * Created by milo on 15-7-30.
 */
object DisruptorDemo {

  class RequestEvent {
    var event: Request[_] = _
    def set(r: Request[_]) { event = r }
  }

  def eventFactory[Event](e: => Event) = new EventFactory[Event] {
    def newInstance(): Event = e
  }

  class LongEventHandler extends EventHandler[RequestEvent] {
    def onEvent(event: RequestEvent, sequence: Long, endOfBatch: Boolean) {
      event.event match {
        case GetUp => println("Event: GetUp")
        case Get(q) => println("Event: Get " + q)
      }
    }
  }

  class LongEventProducer(val ringBuffer: RingBuffer[RequestEvent]) {
    def onData[A](event: Request[A]) = {
      val sequence = ringBuffer.next()
      try {
        val reqEvent = ringBuffer.get(sequence)
        reqEvent.set(event)
      } finally {
        ringBuffer.publish(sequence)
      }
    }
  }

  def main(args: Array[String]) {
    val executor = Executors.newCachedThreadPool()
    val bufferSize = 1024
    val disruptor = new Disruptor[RequestEvent](eventFactory{new RequestEvent()}, bufferSize, executor)
    disruptor.handleEventsWith(new LongEventHandler())
    disruptor.start()
    val ringBuffer = disruptor.getRingBuffer()
    val producer = new LongEventProducer(ringBuffer)
    for (l <- 1 to 1000) {
      producer.onData(GetUp)
      producer.onData(Get("a"))
      Thread.sleep(100)
    }
  }
}
