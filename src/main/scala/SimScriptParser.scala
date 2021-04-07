import java.util
import scala.collection.immutable
import scala.io.Source
import org.yaml.snakeyaml.Yaml

import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

import scala.util.{Failure, Success, Try}

object SimScriptParser {
  def apply(scriptPath: String): Either[String, SimScriptParser] = {
    Try(Source.fromFile(scriptPath).toList.mkString) match {
      case Success(content) => {
        val res = new SimScriptParser(content)
        if (res.yamlParsed.isLeft) Left(res.yamlParsed.left.getOrElse("Yaml parsing error"))
        else Right(res)
      }
      case Failure(exception) => Left(exception.getMessage)
    }
  }
}

class SimScriptParser(content: String) {
  require(content != null)
  private val yaml = new Yaml

  private val yamlParsed: Either[String, Any] = Try(yaml.load(content): Any) match {
    case Success(parser) => Right(parser)
    case Failure(exception) => Left(exception.getMessage)
  }

  def isError: Option[String] = if (yamlParsed.isLeft) Some(yamlParsed.left.getOrElse("Unidentified error during parsing the input program")) else None

  def extractParseTree: Either[String, Either[List[Any], Map[Any, Any]]] = {
    val yamlModel = yamlParsed.getOrElse(throw new Exception(yamlParsed.left.getOrElse("Unexpected exception parsing the input program")))
    if (yamlModel.getClass == classOf[util.ArrayList[_]]) {
      //process a sequence of values, no key/val pairs
      Right(Left(yamlModel.asInstanceOf[java.util.ArrayList[_]].asScala.toList))
    } else if (yamlModel.getClass == classOf[util.LinkedHashMap[_, _]]) {
      //process a list of tuples for key/val pairs
      Right(Right(yamlModel.asInstanceOf[java.util.LinkedHashMap[_, _]].asScala.toMap))
    } else Left(s"Unexpected type in the yaml document: ${yamlModel.getClass}")
  }


  /*
    if(obj.getClass == classOf[util.LinkedHashMap[_,_]]){
      val obj1 =obj.asInstanceOf[java.util.LinkedHashMap[_,_]].asScala.toMap
      val res:Iterable[String] = obj1.map(kv =>{
        kv._1.asInstanceOf[String]
      })
      println(obj)
    } else if(obj.getClass == classOf[util.ArrayList[_]]){
      //          val obj1 =obj.asInstanceOf[java.util.ArrayList[_]]
      println(obj)
    }
    val mapps: immutable.Map[String, Any] = yaml.load(doc1).asInstanceOf[java.util.Map[String, Any]].asScala.toMap
    println(mapps)
  */

}
