import $ivy.{
  `org.apache.spark::spark-core:2.1.0`,
  `org.apache.spark::spark-sql:2.1.0`
}
import ammonite.ops._
import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.sql.SQLContext

val conf = new SparkConf().setMaster("local[*]").setAppName("bxm")
val sc = new SparkContext(conf)
val sqlContext = new SQLContext(sc)

val df = sqlContext.read.json(
  "integration/src/test/resources/ammonite/integration/basic/Spark2.json"
)

df.foreachPartition{records =>
  records.foreach{
    record => println("fake db write")
  }
}