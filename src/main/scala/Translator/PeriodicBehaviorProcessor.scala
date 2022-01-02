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
import Translator.SlanAbstractions.{SlanConstruct, YamlTypes}
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

import scala.collection.mutable

class PeriodicBehaviorProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => (new PeriodicBehaviorKeyParamsProcessor).commandProcessor(convertJ2S(v(0)))
      ::: (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1)))

    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
}