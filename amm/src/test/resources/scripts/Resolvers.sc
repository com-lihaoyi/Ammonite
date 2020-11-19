interp.repositories ++= Seq(coursierapi.IvyRepository.of(
  "https://ambiata-oss.s3-ap-southeast-2.amazonaws.com/[defaultPattern]"
))

@

import $ivy.`com.ambiata::mundane:1.2.1-20141230225616-50fc792`
import com.ambiata.mundane._

