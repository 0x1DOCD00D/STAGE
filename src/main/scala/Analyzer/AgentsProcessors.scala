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

import Analyzer.SlanAbstractions.{Key2Yaml2Construct, SlanConstruct, SlanProcessorSwitch, YamlTypes}
import Analyzer.SlanKeywords.*
import Analyzer.SlantParser.convertJ2S
import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString

class AgentsProcessors extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = super.yamlContentProcessor(yamlObj)
