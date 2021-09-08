/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package HelperUtils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import HelperUtils.ConfigReference.*

class ConfigTestSuite extends AnyFlatSpec with Matchers {
  behavior of "configuration parameters module"
  val config = ConfigReference("stage") match {
    case Some(value) => value
    case None => throw new RuntimeException("Cannot obtain a reference to the config data.")
  }

  it should "obtain the utilization ratio" in {
    config.getDouble("stage.entry1") shouldBe 0.5E0
  }

  it should "obtain the MIPS capacity" in {
    config.getLong("stage.stuff.entry2") shouldBe 100
  }
}
