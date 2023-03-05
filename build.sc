import mill._, scalalib._
import coursier.maven.MavenRepository

val spinalVersion = "1.8.0"

object uestsg extends SbtModule {
  def scalaVersion = "2.11.12"
  override def millSourcePath = os.pwd

  def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      MavenRepository("https://repo.huaweicloud.com/repository/maven/")
    )
  }

  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:$spinalVersion",
    ivy"com.github.spinalhdl::spinalhdl-lib:$spinalVersion"
  )
  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-tester:$spinalVersion")
  }
  def scalacPluginIvyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:$spinalVersion")
}
