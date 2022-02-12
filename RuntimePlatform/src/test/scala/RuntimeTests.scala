/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

//import org.specs2._
import org.specs2.mutable._

class RuntimeTests extends Specification {
  "This is a specification to check the 'Hello world' string".br

  "The 'Hello world' string should" >> {
    "contain 11 characters" >> {
      "Hello world" must haveSize(11)
    }

    "start with 'Hello'" >> {
      "Hello world" must startWith("Hello")
    }

    "end with 'world'" >> {
      "Hello world" must endWith("world")
    }
  }

/*
  def is = s2"""
    This is a specification to check the 'Hello world' string
    The 'Hello world' string should
      contain 11 characters $e1
      start with 'Hello' $e2
      end with 'world' $e3
      """
  def e1 = "Hello world" must haveSize(11)
  def e2 = "Hello world" must startWith("Hello")
  def e3 = "Hello world" must endWith("world")
*/
}