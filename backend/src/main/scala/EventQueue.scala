import zio.stm.{STM, TQueue, TRef, TaskSTM}
import zio.stream.{Take, ZStream}
import zio.{Hub, UIO, URIO, ZEnv, ZHub, ZManaged}
import zio.ZIO

class EventQueue[A] private[EventQueue] (
  hub: Hub[Take[Nothing, A]],
  queue: TQueue[Take[Nothing, A]]
) {
  def offer(a: A): TaskSTM[Unit] = queue.offer(Take.single(a))

  def stream(): ZStream[ZEnv, Nothing, A] = ZStream.fromHub(hub).flattenTake

  def shutdown = (queue.takeAll *> queue.offer(Take.end)).commit.ensuring(hub.shutdown)
}

object EventQueue {
  def make[A]: ZIO[Any, Nothing, EventQueue[A]] =
    (for {
      hub   <- Hub.dropping[Take[Nothing, A]](1024)
      queue <- TQueue.bounded[Take[Nothing, A]](1024).commit
      _     <- ZStream.fromTQueue(queue).flattenTake.intoHub(hub).forkDaemon
    } yield new EventQueue(hub, queue))
}
