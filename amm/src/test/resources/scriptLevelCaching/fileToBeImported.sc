def listScalaFiles = os.list(os.pwd).filter(_.last.endsWith(".sc"))
