/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.{YamlKeyIsNotString, YamlUnexpectedTypeFound}
import Translator.SlanAbstractions.{SlanConstructs, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.Eval
import cats.implicits.*
import cats.kernel.Eq

class ResourceIngesterTableProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => (convertJ2S(v(0)),convertJ2S(v(1))) match {
      case (path2Csv: String, header: Boolean) => Eval.now(List(ResourceCsvTable(List(SlanValue(path2Csv.trim), SlanValue(header)))))
      case (tableName: String, connectionString: String) => Eval.now(List(ResourceDatabaseTable(List(SlanValue(connectionString.trim), SlanValue(tableName.trim)))))
      case unknown => Eval.now(List(YamlUnexpectedTypeFound(unknown.getClass().toString + ": " + unknown.toString)))
    }
    case None => Eval.now(List())
    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
}
