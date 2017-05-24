package ammonite.repl

import java.util.concurrent.ArrayBlockingQueue

class RemoteLogger(sessionId: Long) {
  /**
    * We don't care if anything here crashes, so just swallow the error. Worst
    * come to worst we don't get someone's logs, but that's better than borking
    * their REPL for a remote logger they don't want and never asked for
    */
  def yolo(t: => Unit) = {
    try t
    catch{case e: Throwable => /* YOLO */}
  }

  // Use a bounded queue; we don't want this taking
  // up lots of space if the network doesn't work
  private[this] val inbox = new ArrayBlockingQueue[Option[String]](64)

  private[this] val loggerThread = new Thread(new Runnable {
    override def run() = yolo{

      var done = false

      // Make only one request at a time, and ignore it if it crashes for
      // whatever reason.
      while(!done){

        inbox.take() match{
          case None => done = true
          case Some(msg) =>
            yolo{
              scalaj.http.Http("https://www.google-analytics.com/collect")
                .postForm(Seq(
                  "v" -> "1",
                  "tid" -> "UA-27464920-7",
                  "cid" -> sessionId.toString,
                  "t" -> "pageview",
                  "dp" -> msg
                ))
                .asBytes
            }
        }
      }
    }
  })

  // Low priority thread, and we don't want to keep the JVM around
  // if the thread is still running. This should have as little impact
  // on the main program as possible
  yolo{
    loggerThread.setDaemon(true)
    loggerThread.setPriority(Thread.MIN_PRIORITY)
    loggerThread.start()
  }

  def close() = {
    inbox.offer(None)
  }
  def apply(s: String) = {
    // Don't care if queue is full, just discard it
    inbox.offer(Some(s))
  }
}
