import $ivy.{
  `org.apache.spark::spark-core:2.1.0`,
  `org.apache.spark::spark-sql:2.1.0`
}
import org.apache.spark.sql.SparkSession

val spark = SparkSession.builder.master("local[*]").appName("bxm").getOrCreate

val df = spark.read.json(
  "integration/src/test/resources/ammonite/integration/basic/Spark2.json"
)

df.foreachPartition { records =>
  records.foreach {
    record => println("fake db write")
  }
}
