/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.{SlanUnexpectedTypeFound, YamlKeyIsNotString}
import Translator.SlanAbstractions.{SlanConstruct, YamlPrimitiveTypes, YamlTypes}
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class ResourcesProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case arr: List[_] => List(ResourcePeriodicGenerator((new ResourceGenerationProcessor).commandProcessor(convertJ2S(v(0)))
        ::: (new ResourceStructureProcessor).commandProcessor(convertJ2S(v(1)))))
      case _ => List(Resource((new ResourceTagProcessor).commandProcessor(convertJ2S(v(0))).head,
        (new ResourceStructureProcessor).commandProcessor(convertJ2S(v(1)))))
    }

    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
}