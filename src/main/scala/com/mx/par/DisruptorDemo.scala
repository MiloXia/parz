package com.mx.par

import java.nio.ByteBuffer
import java.util.concurrent.Executors

import com.lmax.disruptor.dsl.Disruptor
import com.lmax.disruptor.{RingBuffer, EventHandler, EventFactory}
import com.mx.par.day06.Request

/**
 * Created by milo on 15-7-30.
 */
object DisruptorDemo {

  trait EventEnvelope[Event[_]] {
    var event: Event[_] = _
    def set(r: Event[_]) { event = r }
  }

  trait GotoWork[A] extends Request[A]
  case object GetUp extends GotoWork[Unit]

  trait IOReq[A] extends Request[A]
  case class Get(q: String) extends IOReq[String]
  case class Put(q: String) extends IOReq[Unit]

  class GotoWorkEvent extends EventEnvelope[GotoWork]
  class IOReqEvent extends  EventEnvelope[IOReq]

  def eventFactory[Event](e: => Event) = new EventFactory[Event] {
    def newInstance(): Event = e
  }

  class LongEventProducer(val ringBuffer: RingBuffer[GotoWorkEvent]) {
    def onData(event: GotoWork[_]) = {
      val sequence = ringBuffer.next()
      try {
        val reqEvent = ringBuffer.get(sequence)
        reqEvent.set(event)
      } finally {
        ringBuffer.publish(sequence)
      }
    }
  }

  class LongEventProducer2(val ringBuffer: RingBuffer[IOReqEvent]) {
    def onData(event: IOReq[_]) = {
      val sequence = ringBuffer.next()
      try {
        val reqEvent = ringBuffer.get(sequence)
        reqEvent.set(event)
      } finally {
        ringBuffer.publish(sequence)
      }
    }
  }

  class LongEventHandler extends EventHandler[GotoWorkEvent] {
    def onEvent(event: GotoWorkEvent, sequence: Long, endOfBatch: Boolean) {
      event.event match {
        case GetUp => println("Event: GetUp")

      }
    }
  }

  class LongEventHandler2 extends EventHandler[IOReqEvent] {
    def onEvent(event: IOReqEvent, sequence: Long, endOfBatch: Boolean) {
      event.event match {
        case Get(q) => println("Event: Get " + q)
        case Put(p) => println("Event: Put" + p)
      }
    }
  }

  def main(args: Array[String]) {
    val executor = Executors.newCachedThreadPool()
    val bufferSize = 1024

    val disruptor = new Disruptor[GotoWorkEvent](eventFactory{new GotoWorkEvent()}, bufferSize, executor)
    disruptor.handleEventsWith(new LongEventHandler())
    disruptor.start()

    val disruptor2 = new Disruptor[IOReqEvent](eventFactory{new IOReqEvent()}, bufferSize, executor)
    disruptor2.handleEventsWith(new LongEventHandler2())
    disruptor2.start()

    val ringBuffer = disruptor.getRingBuffer()
    val producer = new LongEventProducer(ringBuffer)

    val ringBuffer2 = disruptor2.getRingBuffer()
    val producer2 = new LongEventProducer2(ringBuffer2)

    for (l <- 1 to 1000) {
      val l1: List[IOReq[_]] = List(Get("a"), Put("p"))
      val l2: List[GotoWork[_]] = List(GetUp)
      l1.foreach(producer2.onData)
      l2.foreach(producer.onData)
      Thread.sleep(100)
    }
  }
}
