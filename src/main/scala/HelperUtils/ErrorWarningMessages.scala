/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package HelperUtils

import Translator.SlanAbstractions.SlanConstructs
import Translator.SlanConstruct.{IncorrectYamlType, SlanError}
import org.slf4j.Logger

type ErrorOrSlanConstructs = Either[SlanError, SlanConstructs]

object ErrorWarningMessages:
  def DistributionNameFailure(input: String, exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"Incorrect distribution name is specified: $input - $exceptionMessage"
    logger.error(errorMsg)
    SlanError(errorMsg)

  def YamlScriptFileFailure(input: String, exceptionMessage: String)(using logger: Logger): String =
    val errorMsg = s"Error occured when loading input Yaml script $input: $exceptionMessage"
    logger.error(errorMsg)
    errorMsg

  def YamlParsingFailed(exceptionMessage: String)(using logger: Logger): String =
    val errorMsg = s"Error occured when parsing input Yaml script: $exceptionMessage"
    logger.error(errorMsg)
    errorMsg

  def YamlUnexpectedTypeFound(exceptionMessage: String)(using logger: Logger): IncorrectYamlType =
    val errorMsg = s"The following type is not handled in the Yaml script: $exceptionMessage"
    logger.error(errorMsg)
    IncorrectYamlType(errorMsg)

  def SlanUnexpectedTypeFound(exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"The following type $exceptionMessage is not expected at this location in Slan specification:"
    logger.error(errorMsg)
    SlanError(errorMsg)

  def SlanProcessingFailure(path:String, exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"Slan program $path failed for reason $exceptionMessage"
    logger.error(errorMsg)
    SlanError(errorMsg)

  def SlanInvalidConstruct(exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"The following construct $exceptionMessage is not valid at this location in Slan specification:"
    logger.error(errorMsg)
    SlanError(errorMsg)

  def YamlKeyIsNotString(exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"Yaml key $exceptionMessage is not a String"
    logger.error(errorMsg)
    SlanError(errorMsg)

  def YamlKeyIsMissing(exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"Yaml key $exceptionMessage is not specified"
    logger.error(errorMsg)
    SlanError(errorMsg)

  def LogGenericMessage(cls: Class[_], message: String)(using logger: Logger): SlanError =
    val errorMsg = s"${cls.getName}: $message"
    logger.info(errorMsg)
    SlanError(errorMsg)

  def MissingDefinition(exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"Definition $exceptionMessage is not specified"
    logger.error(errorMsg)
    SlanError(errorMsg)

  def IncorrectSlanSpecStructure(exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"Incorrect spec structure with $exceptionMessage"
    logger.error(errorMsg)
    SlanError(errorMsg)
  
  def EmptyWarning(exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"Empty value: $exceptionMessage"
    logger.warn(errorMsg)
    SlanError(errorMsg)
  
  def DuplicateDefinition(exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"Definition $exceptionMessage is already specified"
    logger.error(errorMsg)
    SlanError(errorMsg)

  def WrongCardinality(exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"Incorrect number of entities: $exceptionMessage"
    logger.error(errorMsg)
    SlanError(errorMsg)
  
  def IncorrectParameter(exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"Incorrect parameter is given: $exceptionMessage"
    logger.error(errorMsg)
    SlanError(errorMsg)

  def SeriousInternalError(exceptionMessage: String)(using logger: Logger): SlanError =
    val errorMsg = s"Internal error: $exceptionMessage"
    logger.error(errorMsg)
    SlanError(errorMsg)
