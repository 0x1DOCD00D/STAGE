import akka.actor.ActorSystem
import akka.event.Logging

import java.io.IOException
import java.util
import scala.collection.immutable
/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

import scala.jdk.CollectionConverters.MapHasAsScala

object Main {
  val akkaSystem = ActorSystem("STAGE")

  val logger = Logging(akkaSystem.eventStream, this.getClass.toString)


  def main(args: Array[String]): Unit = {
    val model = SimScriptParser(args(0)) match {
      case Left(error) => {
        logger.error(error)
        akkaSystem.terminate()
        System.exit(1)
      }
      case Right(parser) => parser.extractParseTree match {
        case Left(error) => logger.error(s"Error analyzing yaml: $error")
          System.exit(2)
        case Right(value) => value match {
          case Left(lst) => lst
          case Right(map) => map
        }
      }
    }

    try {
      val inStream = getClass.getClassLoader.getResourceAsStream(args(0));
      val res = getClass.getClassLoader().getResource(args(0));
      if (inStream != null && res != null) {
        val content = new String(inStream.readAllBytes())
        logger.info(content)
        logger.info(s"Located ${args(0)} and loaded it.");


        import org.yaml.snakeyaml.Yaml
        val yaml = new Yaml

        val doc1 =
          """
            |- 1.2
            |- 3.45
            |- 9.19
            |""".stripMargin
        val doc2 =
          """
            |key:
            |  [a : b,20,true,]
            |""".stripMargin

        val doc3 =
          """
            |sequence:
            |- one
            |- two
            |mapping:
            |  ? sky
            |  : blue
            |  sea : green
            |""".stripMargin

        val document =
          """
            |flowseq: [one, 2, 3.14,[nested1, [nested2, nkey:[10, [oy, wey]]]]]
            |# some comment
            |? Mark McGwire
            |? Sammy Sosa
            |? Ken Griff
            |Stack:
            |  - file: TopClass.py
            |    line: 23
            |    code: |
            |      x = MoreObject("345\n")
            |  - file: MoreClass.py
            |    line: 58
            |    code: |-
            |      foo = bar
            |exponential: 12.3015e+02
            |timestamp: 2001-12-14 21:59:43.10 -5
            |emptykey:
            |american:
            |  - Boston Red Sox
            |  - Detroit Tigers
            |  - New York Yankees
            |national:
            |  - New York Mets
            |  - Chicago Cubs
            |  - Atlanta Braves
            |invoice: 34843
            |date   : 2001-01-23
            |billTo: &id001
            |    given  : Chris
            |    family : Dumars
            |    id : 1
            |    address:
            |        lines: |
            |            458 Walkman Dr.
            |            Suite #292
            |        city    : Royal Oak
            |        state   : MI
            |        postal  : 48046
            |shipTo: *id001
            |product:
            |    - sku         : BL394D
            |      quantity    : 4
            |      description : Basketball
            |      price       : 450.00
            |    - sku         : BL4438H
            |      quantity    : 1
            |      description : Super Hoop
            |      price       : 2392.00
            |tax  : 251.42
            |total: 4443.52
            |comments:
            |    Late afternoon is best.
            |    Backup contact is Nancy
            |    Billsmer @ 338-4338.""".stripMargin

        val prim: Any = yaml.load(doc3)
        val obj: Any = yaml.load(document)
        if (obj.getClass == classOf[util.LinkedHashMap[_, _]]) {
          val obj1 = obj.asInstanceOf[java.util.LinkedHashMap[_, _]].asScala.toMap
          val res: Iterable[String] = obj1.map(kv => {
            kv._1.asInstanceOf[String]
          })
          println(obj)
        } else if (obj.getClass == classOf[util.ArrayList[_]]) {
          //          val obj1 =obj.asInstanceOf[java.util.ArrayList[_]]
          println(obj)
        }
        val mapps: immutable.Map[String, Any] = yaml.load(doc1).asInstanceOf[java.util.Map[String, Any]].asScala.toMap
        println(mapps)
        mapps.foreach(kv => {
          println(s"key: ${kv._1} and the type of value is ${kv._2.getClass.toString}")
          /*          case kv._2.getClass.toString match {
                      case "java.util.LinkedHashMap" =>
                    }*/
          /*
                    classOf[java.util.LinkedHashMap]
                    kv._2 match {
                      case v:java.util.LinkedHashMap[String] =>println()
                    }
          */
        })

        /*
        //        val lst: immutable.Map[String, Any] = yaml.loadAll(somedoc).asInstanceOf[java.util.Map[String, Any]].asScala.toMap

                val mapps: immutable.Map[String, Any] = yaml.load(document).asInstanceOf[java.util.Map[String, Any]].asScala.toMap
                val lst: java.lang.Iterable[Object] = yaml.loadAll(somedoc).asInstanceOf[java.lang.Iterable[Object]]
                val mapps: Object = yaml.load(document).asInstanceOf[Object]
                println(mapps.getClass.getName)
                println(lst.getClass.getName)
                lst.forEach((o:Object) =>println(o.getClass.getName))
        */

        /*
                import io.circe.{yaml, _}
                val json = yaml.parser.parse(document)
                logger.info(s"Yaml-json content: ${json.toString}")
                val cursor: HCursor = json.toOption.get.hcursor
                cursor.keys.foreach(key =>{
                  val vkeys =  key.toList
                  vkeys.foreach(key =>{
                    cursor.downField(key).i
                  }
              })
        */
        /*
                cursor.downN(0).values.get.foreach(j =>{
                  if(j.isObject) println("object " + j.toString())
                  if(j.isArray) println("array " + j.toString())
                  if(j.isString) println("string " + j.toString())
                  if(j.isNumber) println("number " + j.toString())
                }
                )
        */
        //        cursor.values.foreach(println)
        /*
                val va: Vector[Json] = cursor.value.asArray.get
                logger.info("--------------------------------------")
                va map (v =>
                  logger.info(v.asArray.toString))
                //        println(cursor.get[String]("foo"))
        */
      }
      else logger.error("Test.yaml is not on classpath");
    } catch {
      case e: IOException => logger.error("Exception ", e.getMessage());
    }
    akkaSystem.terminate()
  }
}
