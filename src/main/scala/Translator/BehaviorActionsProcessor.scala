/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */
package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlPrimitiveTypes, YamlTypes}
import Translator.SlanKeywords.{ExternalService, FnPrefix, IF, NOT}
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class BehaviorActionsProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case entry: String if entry.toUpperCase === NOT.toUpperCase => List(Not((new NotProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === IF.toUpperCase => List(IfThenElse((new IfThenElseProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase.startsWith(FnPrefix.toUpperCase) => (new FunctionProcessor).commandProcessor(convertJ2S(v))
      case period: Map[_, _] if period.size == 1 => //only one entry is allowed for intervalTime: howManytimes
        (convertJ2S(period.toList.head._1), convertJ2S(period.toList.head._2)) match {
        case (timeInterval, iterations): (YamlPrimitiveTypes, YamlPrimitiveTypes) => List(PeriodParameters(SlanValue(timeInterval), SlanValue(iterations))) ::: (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))
        case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass.toString)).constructSlanRecord
      }
      //some custom external function
      //TODO: implement an external function processor
      case entry: String => (new FunctionProcessor).commandProcessor(convertJ2S(v))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }

    case entry: YamlPrimitiveTypes => List(SlanValue(entry))

    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
