/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

/*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RuntimeTests extends AnyFlatSpec with Matchers {
  behavior of "random number generator"
  val config: Config = ConfigReference("Stage.Distributions") match {
    case Some(value) => value
    case None => throw new RuntimeException("Cannot obtain a reference to the config data for PDFs.")
  }

  it should "create hash codes from arrays of doubles" in {
    MurmurHash3.orderedHash(Array(1.12, 2.87, 52, 187)) shouldBe MurmurHash3.orderedHash(Array(1.12, 2.87, 52, 187))
    MurmurHash3.orderedHash(Array(1.12, 2.87, 52, 187)) should not be MurmurHash3.orderedHash(Array(1.12, 2.871, 52, 187))
  }
*/
