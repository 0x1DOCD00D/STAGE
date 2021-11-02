/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package HelperUtils

import org.slf4j.Logger

object ErrorWarningMessages:
  def YamlScriptFileFailure[T](input: String, exceptionMessage: String)(using logger: Logger): String =
    val errorMsg = s"Error occured when loading input Yaml script $input: $exceptionMessage"
    logger.error(errorMsg)
    errorMsg

  def YamlParsingFailed(exceptionMessage: String)(using logger: Logger): String =
    val errorMsg = s"Error occured when parsing input Yaml script: $exceptionMessage"
    logger.error(errorMsg)
    errorMsg

  def YamlUnexpectedTypeFound(exceptionMessage: String)(using logger: Logger): String =
    val errorMsg = s"The following type is not handled in the Yaml script: $exceptionMessage"
    logger.error(errorMsg)
    errorMsg

  def SlanUnexpectedTypeFound(exceptionMessage: String)(using logger: Logger): String =
    val errorMsg = s"The following type $exceptionMessage is not expected at this location in Slan specification:"
    logger.error(errorMsg)
    errorMsg

  def YamlKeyIsNotString(exceptionMessage: String)(using logger: Logger): String =
    val errorMsg = s"Yaml key $exceptionMessage is not a String"
    logger.error(errorMsg)
    errorMsg

  def LogGenericMessage(cls: Class[_], message: String)(using logger: Logger): String =
    val errorMsg = s"${cls.getName}: $message"
    logger.info(errorMsg)
    errorMsg
