import ammonite.ops._
def listScalaFiles = ls(pwd) |? (_.last.endsWith(".sc"))