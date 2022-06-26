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
import HelperUtils.ErrorWarningMessages.{DuplicateDefinition, IncorrectParameter, MissingDefinition}
import SlanIR.{EntityId, EntityOrError}
import Translator.SlanConstruct.SlanError
import org.slf4j.Logger

import scala.collection.mutable.Map

trait Resource extends SlanEntity

case class ProducerConsumer(id: EntityId, storage: ResourceStorage, composites: Option[List[Resource]]) extends SlanEntity(id), Resource

case class Generator(id: EntityId, pdf: String) extends SlanEntity(id), Resource

object Resource:
  private val bookkeeper = new EntityBookkeeper[Resource]
  def apply(id: EntityId): Option[Resource] = bookkeeper.get(id).asInstanceOf

  def apply(id: EntityId, storageOrPdf: String, composites: Option[List[Resource]], inits: Option[List[ValueInResource[Long]]]): EntityOrError[Resource] =
    if bookkeeper.contains(id) then
      DuplicateDefinition(s"resource $id")
    else
      ResourceStorage(storageOrPdf) match
           case ResourceStorage.UNRECOGNIZED =>
              if PDFs.PdfStreamGenerator.listOfSupportedDistributions.contains(storageOrPdf.toUpperCase) then
                bookkeeper.set(id, Generator(id, storageOrPdf.toUpperCase))
              else
                IncorrectParameter(s"$storageOrPdf in resource definition")
           case rs => bookkeeper.set(id, ProducerConsumer(id, rs, composites))
