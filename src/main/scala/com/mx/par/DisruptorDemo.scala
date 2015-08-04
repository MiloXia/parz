package com.mx.par

import java.util.concurrent.{ExecutorService, Executors}

import com.lmax.disruptor.dsl.Disruptor
import com.lmax.disruptor.{RingBuffer, EventHandler, EventFactory}
import com.mx.par.day06.Request

/**
 * Created by milo on 15-7-30.
 */
object DisruptorDemo {

  trait EventEnvelope[Event[_] <: Request[_]] {
    var event: Event[_] = _
    def set(r: Event[_]) { event = r }
  }

  class GotoWork[A] extends Request[A] with EventEnvelope[GotoWork]
  case object GetUp extends GotoWork[Unit]

  class IOReq[A] extends Request[A] with EventEnvelope[IOReq]
  case class Get(q: String) extends IOReq[String]
  case class Put(q: String) extends IOReq[Unit]

//  class GotoWorkEvent extends EventEnvelope[GotoWork]
//  class IOReqEvent extends  EventEnvelope[IOReq]

  def eventFactory[Event](e: => Event) = new EventFactory[Event] {
    def newInstance(): Event = e
  }

  class LongEventProducer[EE[_] <: Request[_] with EventEnvelope[EE]](val ringBuffer: RingBuffer[EE[_]]) {
    def onData(event: EE[_]) = {
      val sequence = ringBuffer.next()
      try {
        val reqEvent = ringBuffer.get(sequence)
        reqEvent.set(event)
      } finally {
        ringBuffer.publish(sequence)
      }
    }
  }

  object LongEventProducer {
    def apply[EE[_] <: Request[_] with EventEnvelope[EE]](ringBuffer: RingBuffer[EE[_]]) = new LongEventProducer[EE](ringBuffer)
  }

  def getDisruptor[EE[_] <: Request[_] with EventEnvelope[EE]](e: => EE[_], bufferSize: Int, executor: ExecutorService) = new Disruptor[EE[_]](eventFactory(e), bufferSize, executor)

  class LongEventHandler extends EventHandler[GotoWork[_]] {
    def onEvent(event: GotoWork[_], sequence: Long, endOfBatch: Boolean) {
      println(Thread.currentThread().getName)
      event.event match {
        case GetUp => println("Event: GetUp")

      }
    }
  }

  class LongEventHandler2 extends EventHandler[IOReq[_]] {
    def onEvent(event: IOReq[_], sequence: Long, endOfBatch: Boolean) {
      println(Thread.currentThread().getName)
      event.event match {
        case Get(q) => println("Event: Get " + q)
        case Put(p) => println("Event: Put" + p)
      }
    }
  }

  def main(args: Array[String]) {
    val executor = Executors.newCachedThreadPool()
    val bufferSize = 1024

    val disruptor = getDisruptor(new GotoWork(), bufferSize, executor)
    disruptor.handleEventsWith(new LongEventHandler())
    disruptor.start()

    val disruptor2 = getDisruptor(new IOReq(), bufferSize, executor)
    disruptor2.handleEventsWith(new LongEventHandler2())
    disruptor2.start()

    val producer = LongEventProducer[GotoWork](disruptor.getRingBuffer())

    val producer2 = LongEventProducer[IOReq](disruptor2.getRingBuffer())

    for (l <- 1 to 10) {
      val l1: List[IOReq[_]] = List(Get("a"), Put("p"))
      val l2: List[GotoWork[_]] = List(GetUp)
      l1.foreach(producer2.onData)
      l2.foreach(producer.onData)
      Thread.sleep(100)
    }
  }
}
