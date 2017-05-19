interp.repositories() ++= Seq(coursier.ivy.IvyRepository.fromPattern(
  "https://ambiata-oss.s3-ap-southeast-2.amazonaws.com/" +:
    coursier.ivy.Pattern.default
))

import $ivy.`com.ambiata::mundane:1.2.1-20141230225616-50fc792`
import com.ambiata.mundane._

