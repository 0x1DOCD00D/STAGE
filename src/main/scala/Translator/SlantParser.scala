/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package Translator

import HelperUtils.{CreateLogger, ErrorWarningMessages}
import Translator.SlanAbstractions.*
import cats.effect.kernel.Sync
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.instances.either.*
import cats.syntax.all.*
import cats.{Eval, MonadError}
import org.joda.time.DateTime
import org.slf4j.Logger
import org.snakeyaml.engine.*
import org.snakeyaml.engine.v2.*
import org.snakeyaml.engine.v2.api.*

import java.util.*
import scala.collection.immutable
import scala.io.{BufferedSource, Source}
import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
import scala.runtime.Nothing$
import scala.util.{Failure, Success, Try}

trait SlanProgramFailed
trait SlanProgramPassed
final case class ErrorSlanInput(msg:String) extends SlanProgramFailed
final case class FileRuntimeError(e: Throwable) extends SlanProgramFailed
case class ContentSlanProgram(program:String) extends SlanProgramPassed:
  require(program != null)
case class SlanYamlHandle(yaml: Any) extends SlanProgramPassed
//  require(yaml != null)

type ParsingPassedOrFailed = Either[SlanProgramFailed, SlanProgramPassed]
type SlanProgramParsingResult = IO[ParsingPassedOrFailed]

given logger: Logger = CreateLogger(classOf[SlantParser.type])

object SlantParser:
  def apply(inputPath: String): SlanProgramParsingResult =
    require(inputPath != null)
    logger.info(s"Input Slan specification is specified as $inputPath")
    processSlanProgram(inputPath)

  private def CreateSlanInputResource(inputPath: String): Resource[IO, BufferedSource] =
    Resource.make {
      IO.blocking { Source.fromFile(inputPath) }
    } {
      file => IO.blocking { file.close()  }
    }

  private def processSlanProgram(inputPath: String): SlanProgramParsingResult = for {
      result <- CreateSlanInputResource(inputPath).use {
        file => IO.blocking {
          ContentSlanProgram(file.toList.mkString)
        }
      }.attempt.map(_.leftMap(FileRuntimeError.apply))
      output <- parseSlanProgram(result)
    } yield output

  private def parseSlanProgram(content: ParsingPassedOrFailed): SlanProgramParsingResult =
    content match
      case Left(err) => IO(content)
      case Right(slanContent) => IO.delay(yamlModel(slanContent))

  private def yamlModel(yamlprg: SlanProgramPassed): ParsingPassedOrFailed =
    yamlprg match
      case ContentSlanProgram(program) => Try(new Load(LoadSettings.
        builder.
        setAllowDuplicateKeys(true).
        setAllowRecursiveKeys(true).
        build).loadFromString(program): Any) match {
          case Success(parser) => SlanYamlHandle(parser).asRight
          case Failure(exception) => ErrorSlanInput(ErrorWarningMessages.YamlParsingFailed(exception.getMessage)).asLeft
      }
      case _ => ErrorSlanInput(ErrorWarningMessages.YamlParsingFailed(yamlprg.toString)).asLeft


  private[Translator] def convertJ2S(yamlInstance: Any): YamlTypes = yamlInstance match {
    case v: YamlTypes => v
    case null => None
    case _ => yamlInstance.getClass match {
      case c if c == classOf[java.util.ArrayList[_]] => yamlInstance.asInstanceOf[java.util.ArrayList[_]].asScala.toList
      case c if c == classOf[java.util.LinkedHashMap[_, _]] => yamlInstance.asInstanceOf[java.util.LinkedHashMap[_, _]].asScala.toMap
      case c if c == classOf[(_, _)] => yamlInstance.asInstanceOf[(_, _)]
      case c if c == classOf[java.lang.String] => yamlInstance.asInstanceOf[String]
      case c if c == classOf[java.lang.Integer] => yamlInstance.asInstanceOf[Int]
      case c if c == classOf[java.lang.Double] => yamlInstance.asInstanceOf[Double]
      case c if c == classOf[java.lang.Boolean] => yamlInstance.asInstanceOf[Boolean]
      case c if c == classOf[Date] => new DateTime(yamlInstance.asInstanceOf[Date])
      case c => ErrorWarningMessages.YamlUnexpectedTypeFound(c.getName)
    }
  }