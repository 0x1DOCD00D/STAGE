/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.{SlanUnexpectedTypeFound, YamlKeyIsNotString}
import Translator.SlanAbstractions.{SlanConstruct, YamlPrimitiveTypes, YamlTypes}
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class PeriodicBehaviorKeyParamsProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case timeInterval: YamlPrimitiveTypes => List(PeriodicBehaviorFiringDuration(SlanValue(timeInterval), None))
    case v: (_, _) => (convertJ2S(v(0)), convertJ2S(v(1))) match {
      case (timeInterval: YamlPrimitiveTypes, timesX: YamlPrimitiveTypes) => List(PeriodicBehaviorFiringDuration(SlanValue(timeInterval), Some(SlanValue(timesX))))
      case (None, timesX: YamlPrimitiveTypes) => List(PeriodicBehaviorFiringDuration(SlanValue(0), Some(SlanValue(timesX))))
      case (None, None) => List(PeriodicBehaviorFiringDuration(SlanValue(0), None))
      case (timeInterval: YamlPrimitiveTypes, None) => List(PeriodicBehaviorFiringDuration(SlanValue(timeInterval), None))
      case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass.toString)).constructSlanRecord
    }
    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
}