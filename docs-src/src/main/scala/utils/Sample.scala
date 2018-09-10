package utils

object Sample {

  def curlCommand(curlUrl: String) =
    s"""sudo sh -c '(echo "#!/usr/bin/env sh" && curl -L """ +
      curlUrl +
      ") > /usr/local/bin/amm && chmod +x /usr/local/bin/amm' && amm"

  val replCurl     = curlCommand(ammonite.Constants.curlUrl)
  val unstableCurl = curlCommand(ammonite.Constants.unstableCurlUrl)

  val cygwinSed =
    """$ sed -i '0,/"\$0"/{s/"\$0"/`cygpath -w "\$0"`/}' /usr/local/bin/amm"""

  val filesystemCurl =
    "$ mkdir -p ~/.ammonite && curl -L -o ~/.ammonite/predef.sc https://git.io/vHaKQ"

}
