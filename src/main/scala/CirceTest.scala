/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *   
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *  
 */

object CirceTest extends App {

  import cats.syntax.either._
  import io.circe._, io.circe.parser._

  val json: String =
    """
  {
    "id": "c730433b-082c-4984-9d66-855c243266f0",
    "name": "Foo",
    "counts": [1, 2, 3],
    "values": {
      "bar": true,
      "baz": 100.001,
      "qux": ["a", "b"]
    }
  }
"""

  val doc: Json = parse(json).getOrElse(Json.Null)
  val cursor: HCursor = doc.hcursor

  val baz: Decoder.Result[Double] =
    cursor.downField("values").downField("baz").as[Double]
  // baz: Decoder.Result[Double] = Right(100.001)

  // You can also use `get[A](key)` as shorthand for `downField(key).as[A]`
  val baz2: Decoder.Result[Double] =
    cursor.downField("values").get[Double]("baz")
  // baz2: Decoder.Result[Double] = Right(100.001)

  val secondQux: Decoder.Result[String] =
    cursor.downField("values").downField("qux").downArray.as[String]
}
