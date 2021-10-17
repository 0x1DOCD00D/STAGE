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

import Analyzer.SlantParser.convertJ2S
import SlanAbstractions.*
import SlanKeywords.*
import HelperUtils.ErrorWarningMessages.*
import cats.implicits._
import cats.kernel.Eq
import AgentsProcessors.agentsProcessor
//import MessageProcessors.messagesProcessor
//import ModelsProcessors.modelsProcessor
import SlanCommonUtilities.*

object SlanTranslator extends GenericSwitchProcessor :
  private[Analyzer] val slanStructure: SlanProcessorSwitch = Map(Agents.toLowerCase -> agentsProcessor) //, Messages.toLowerCase -> messagesProcessor, Models.toLowerCase -> modelsProcessor)
