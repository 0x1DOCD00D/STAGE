/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package Analyzer

object SlanAbstractions:
  trait SlanConstruct

  type YamlTypes = List[_] | Map[_, _] | Tuple2[_, _] | String | Int | Double | Boolean | Option[_]
  type Yaml2Construct = YamlTypes => List[SlanConstruct]
  type Key2Yaml2Construct = Option[String] => Yaml2Construct
  type SlanProcessorSwitch = Map[String, Key2Yaml2Construct]
