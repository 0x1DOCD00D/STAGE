/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.*
import HelperUtils.ExtentionMethods.*
import Translator.SlanAbstractions.{MessageReference, SlanConstructs}
import Translator.SlanConstruct.{MessageDeclaration, Resources, SlanError}
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import cats.syntax.*
import cats.{Eq, Semigroup}

object SlanMessages2IR extends (List[Translator.SlanConstruct.Message] => SlanEntityValidated[Map[EntityId, MessageIR]]):

  override def apply(slanMsgs: List[Translator.SlanConstruct.Message]): SlanEntityValidated[Map[EntityId, MessageIR]] =
    val msgMap: Map[MessageReference, MessageIR] = slanMsgs.foldLeft(Map[MessageReference, MessageIR]()){
      (map, elem) =>
        val msgDecl: MessageDeclaration = elem.id.asInstanceOf[List[MessageDeclaration]].head
        map + (msgDecl.id -> MessageIR(msgDecl.id, msgDecl.parent, ResourceIR(elem.fields)))
    }

    msgMap.foldLeft(Map[MessageReference, MessageIR]()) {
      (map, elem) =>
        elem._2.parent match
          case None => map + elem
          case Some(v: MessageReference) =>
            if msgMap.contains(v) then map + (elem._1 -> elem._2.withParent(msgMap.get(v)))
            else map + (elem._1 -> elem._2.withParent(SeriousInternalError(s"parent message $v is not defined")))
          case Some(v: MessageIR) => map + elem
          case err: SlanError => map + elem
    }.validNel
