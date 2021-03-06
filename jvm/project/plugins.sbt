resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.2.0")

addSbtPlugin("org.scalaxb" % "sbt-scalaxb" % "1.0.1")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.1")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.2.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-start-script" % "0.7.0")