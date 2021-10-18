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

import Analyzer.SlanAbstractions.{SlanConstruct, YamlTypes}
import Analyzer.SlantParser.convertJ2S
import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString

class UnknownEntryProcessor(val yamlObj: YamlTypes, val key: Option[String] = None):
  def constructSlanRecord: List[SlanConstruct] = List(UnknownConstruct(key.getOrElse("No key provided"), yamlObj.getClass().toString, yamlObj.toString))
