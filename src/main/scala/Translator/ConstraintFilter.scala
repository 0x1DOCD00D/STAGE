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

import Translator.SlanAbstractions.{SlanConstruct, YamlPrimitiveTypes}

/*
* Describes an action of filtering out values based on some relational operator
*     MixFrequence: {
                  Rate: Normal, #create three probabilities for three messages
                  Parameters: {
                    1: 0.6,
                    2: 0.2
                  },
                  Seed: 200,
                  Constraints: [">=": 0, "<": 1]
                }
* */

object ConstraintFilter:
  def apply(key: String, v: YamlPrimitiveTypes): ConstraintFilter = key match {
    case "<" => LESS(SlanValue(v))
    case "<=" => LESSEQUAL(SlanValue(v))
    case ">" => GREATER(SlanValue(v))
    case ">=" => GREATEREQUAL(SlanValue(v))
    case "=" => EQUAL(SlanValue(v))
    case "==" => EQUAL(SlanValue(v))
    case "===" => EQUAL(SlanValue(v))
    case _ => UNDEFINED(SlanValue(v))
  }

enum ConstraintFilter(val boundary: SlanValue) extends SlanConstruct :
  case LESS(v: SlanValue) extends ConstraintFilter(v)
  case LESSEQUAL(v: SlanValue) extends ConstraintFilter(v)
  case GREATER(v: SlanValue) extends ConstraintFilter(v)
  case GREATEREQUAL(v: SlanValue) extends ConstraintFilter(v)
  case EQUAL(v: SlanValue) extends ConstraintFilter(v)
  case UNDEFINED(v: SlanValue) extends ConstraintFilter(v)
