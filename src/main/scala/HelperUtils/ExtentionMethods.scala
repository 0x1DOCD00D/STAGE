/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package HelperUtils

import cats.Eq
import cats.implicits.*
import cats.syntax.*

object ExtentionMethods:
  extension [T](p: List[T])
    def containsHeadOnly: Boolean =
      p.length === 1

  extension (p: Int)
    def existsOne: Boolean =
      p === 1