/*
 * Copyright (c) 2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstructs, YamlPrimitiveTypes, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlantParser.convertJ2S
import cats.Eval
import cats.implicits.*
import cats.kernel.Eq

class BehaviorsProcessor extends GenericProcessor:
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case cv: String if lookAhead4Periodic(convertJ2S(v(1))) => Eval.now(List(PeriodicBehavior(cv.trim, (new PeriodicBehaviorProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case cv: String => Eval.now(List(Behavior(cv.trim, (new MessageResponseBehaviorProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case unknown => Eval.now(List(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString)))
    }

    case None => Eval.now(List())

    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }

  private def lookAhead4Periodic(yamlObj: YamlTypes): Boolean =
    (new PeriodicBehaviorLookaheadProcessor).commandProcessor(convertJ2S(yamlObj)).value match
      case List(SlanValue(true)) => true
      case List(SlanValue(false)) => false
      case _ => false
