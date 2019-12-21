import mill._
import mill.scalalib._

object scalalr extends ScalaModule {
    def scalaVersion = "2.12.8"

    def ivyDeps = Agg(ivy"com.lihaoyi::sourcecode::0.1.9")
    // def mainClass = Some("secc.SecC")

    object test extends Tests {
        def ivyDeps = Agg(ivy"io.monix::minitest:2.7.0")
        def testFrameworks = Seq("minitest.runner.Framework")
    }
}
