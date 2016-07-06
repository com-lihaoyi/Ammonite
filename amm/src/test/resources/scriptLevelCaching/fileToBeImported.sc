import ammonite.ops._
def listScalaFiles = ls(cwd) |? (_.last.endsWith(".sc"))