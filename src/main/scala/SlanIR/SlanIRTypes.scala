/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.CreateLogger
import Translator.SlanAbstractions.StorageTypeReference
import Translator.SlanConstruct.SlanError
import cats.Semigroup
import cats.data.{NonEmptyList, ValidatedNel}
import cats.instances.string.*
import cats.syntax.all.catsSyntaxSemigroup
import org.slf4j.Logger

given logger:Logger = CreateLogger(classOf[SlanEntity])
given slanErrorSemigroup: Semigroup[SlanError] = Semigroup.instance[SlanError] { (e1, e2) =>
  SlanError(e1.errorMessage |+| e2.errorMessage)
}

type SlanEntityValidated = [T] =>> ValidatedNel[SlanError, T]
type EntityId = String
type CollectionOfEntities = List[EntityId]
type Likelihood = Double
type PdfGenerator = Generator | Likelihood
type AgentId = String
type StateId = String
type CorrelationId = String
type Cardinality = Int | EntityId
type SlanEntityInstance = (SlanEntity, Cardinality)
type EntityOrError = [E] =>> E | SlanError

type MessageTriple = (EntityId, Option[EntityId], Option[List[Resource]])
type MessagePair = (EntityId, Option[EntityId])
type MessageTripleCollection = List[MessageTriple]
type CollectionOfMessages = List[Message]
