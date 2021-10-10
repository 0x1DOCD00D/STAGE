/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package Analyzer

import Analyzer.SlantParser.ReturnTypes
import Generator.PDFs.PdfStreamGenerator
import HelperUtils.CreateLogger

import java.util
import scala.collection.immutable
import scala.io.Source
import org.yaml.snakeyaml.Yaml
import HelperUtils.ErrorWarningMessages
import org.slf4j.Logger

import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
import scala.runtime.Nothing$
import scala.util.{Failure, Success, Try}

given logger: Logger = CreateLogger(classOf[SlantParser])

class SlantParser(content: String):
  require(content != null)
  private val yaml = new Yaml

  private val yamlModel = Try(yaml.load(content): Any) match {
    case Success(parser) => parser
    case Failure(exception) => {
      throw new IllegalArgumentException(ErrorWarningMessages.YamlParsingFailed(exception.getMessage))
    }
  }

  private def extractParseTree: ReturnTypes =
    if yamlModel == null then Option(null)
    else yamlModel.getClass match {
      case c if c == classOf[java.util.ArrayList[_]] => yamlModel.asInstanceOf[java.util.ArrayList[_]].asScala.toList
      case c if c == classOf[java.util.LinkedHashMap[_, _]] => yamlModel.asInstanceOf[java.util.LinkedHashMap[_, _]].asScala.toMap
      case c if c == classOf[java.lang.String] => yamlModel.asInstanceOf[String]
      case c if c == classOf[java.lang.Integer] => yamlModel.asInstanceOf[Int]
      case c if c == classOf[java.lang.Double] => yamlModel.asInstanceOf[Double]
      case c if c == classOf[java.lang.Boolean] => yamlModel.asInstanceOf[Boolean]
      case c => throw new RuntimeException(ErrorWarningMessages.YamlUnexpectedTypeFound(c.getName))
    }

object SlantParser:
  type ReturnTypes = List[_] | Map[_, _] | String | Int | Double | Boolean | Option[_]

  def apply(inputPath: String): ReturnTypes =
    Try(Source.fromFile(inputPath).toList.mkString) match {
      case Success(content) => (new SlantParser(content)).extractParseTree
      case Failure(exception) => {
        throw new IllegalArgumentException(ErrorWarningMessages.YamlScriptFileFailure(inputPath, exception.getMessage))
      }
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


