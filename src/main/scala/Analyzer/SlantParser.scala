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

import SlanAbstractions.*
import Generator.PDFs.PdfStreamGenerator
import HelperUtils.CreateLogger
import SlanAbstractions.*

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

  val yamlModel: Any = Try(yaml.load(content): Any) match {
    case Success(parser) => parser
    case Failure(exception) => throw new IllegalArgumentException(ErrorWarningMessages.YamlParsingFailed(exception.getMessage))
  }

object SlantParser:
  def apply(inputPath: String): SlantParser =
    Try(Source.fromFile(inputPath)) match {
      case Success(file) =>
        val content = file.toList.mkString
        file.close()
        new SlantParser(content)

      case Failure(exception) => throw new IllegalArgumentException(ErrorWarningMessages.YamlScriptFileFailure(inputPath, exception.getMessage))
    }

  private[Analyzer] def convertJ2S(yamlInstance: Any): YamlTypes =
    if yamlInstance == null then None
    else if yamlInstance.isInstanceOf[YamlTypes] then yamlInstance.asInstanceOf[YamlTypes]
    else yamlInstance.getClass match {
      case c if c == classOf[java.util.ArrayList[_]] => yamlInstance.asInstanceOf[java.util.ArrayList[_]].asScala.toList
      case c if c == classOf[java.util.LinkedHashMap[_, _]] => yamlInstance.asInstanceOf[java.util.LinkedHashMap[_, _]].asScala.toMap
      case c if c == classOf[(_, _)] => yamlInstance.asInstanceOf[(_, _)]
      case c if c == classOf[java.lang.String] => yamlInstance.asInstanceOf[String]
      case c if c == classOf[java.lang.Integer] => yamlInstance.asInstanceOf[Int]
      case c if c == classOf[java.lang.Double] => yamlInstance.asInstanceOf[Double]
      case c if c == classOf[java.lang.Boolean] => yamlInstance.asInstanceOf[Boolean]
      case c => throw new RuntimeException(ErrorWarningMessages.YamlUnexpectedTypeFound(c.getName))
    }