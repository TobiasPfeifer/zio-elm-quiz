import zio.stm.{STM, TQueue, TRef, TaskSTM}
import zio.stream.{Take, ZStream}
import zio.{Hub, UIO, URIO, ZEnv, ZHub, ZManaged}

class EventQueue[A] private[EventQueue] (
  private val queue: TQueue[A],
  private val done: TRef[Boolean],
  private val hub: Hub[Take[Nothing, A]]
) {
  def offer(a: A): TaskSTM[Unit] =
    for {
      done <- done.get
      _ <- if (!done) queue.offer(a) else STM.fail(new IllegalStateException("EventQueue has already been shut down"))
    } yield ()

  def stream(): ZStream[ZEnv, Nothing, A] = ZStream.fromHubWithShutdown(hub).map(_.exit).collectSuccess.flattenChunks

  def shutdown(): UIO[Unit] = done.set(true).commit *> queue.isEmpty.retryUntil(identity).commit *> hub.shutdown
}

object EventQueue {
  def make[A]: URIO[Any, EventQueue[A]] =
    (for {
      queue <- TQueue.bounded[A](100).commit
      hub   <- ZHub.dropping[Take[Nothing, A]](1000)
      done  <- TRef.make(false).commit

      _ <- ZStream.fromTQueue(queue).intoHub(hub).forkDaemon

      //        pullFromQueue = (for {
      //                          isEmpty  <- queue.isEmpty
      //                          elements <- if (isEmpty) queue.take.map(List(_)) else queue.takeAll
      //                        } yield elements).commit.timeout(30.second).someOrElse(List.empty[A])
      //
      //        loop = for {
      //                 elements <- pullFromQueue
      //                 _        <- hub.publishAll(elements)
      //                 _        <- ZIO.yieldNow
      //                 stop     <- done.get
      //               } yield stop
      //
      //        shutdown        = hub.shutdown
      //        handleRemainder = queue.takeAll.commit.flatMap(hub.publishAll(_))
      //        _              <- loop.repeatUntil(_ == true).ensuring(handleRemainder).ensuring(shutdown).forkDaemon
    } yield new EventQueue(queue, done, hub))
}
