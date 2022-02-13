/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import com.github.nscala_time.time.Imports.*

object SlanAbstractions:
  import SlanConstruct.*

  type YamlPrimitiveTypes = String | Int | Double | Boolean | Long | DateTime
  type YamlTypes = List[_] | Map[_, _] | Tuple2[_, _] | YamlPrimitiveTypes | Option[_]
  type BehaviorReference = Option[String]
  type BehaviorMandatoryReference = String
  type StateReference = Option[String]
  type MessageReference = String
  type ModelReference = String
  type ModelGraphReference = String
  type PdfReference = String
  type PdfParameter = Double
  type AgentReference = String
  type GroupReference = String
  type ConsistencyModelReference = String
  type ChannelReference = String
  type ResourceReference = String
  type StorageTypeReference = Option[String]
  type GroupLeaderReference = String
  type ParameterOrderNumber = Int
  type SlanConstructs = List[SlanConstruct]
  type SlanValues = List[SlanValue]
  type SlanKeyValues = List[SlanKeyValue]