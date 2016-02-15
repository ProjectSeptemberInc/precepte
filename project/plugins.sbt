resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.9")

addSbtPlugin("com.frugalmechanic" % "fm-sbt-s3-resolver" % "0.4.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.3.2")

addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.4.0")

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")
