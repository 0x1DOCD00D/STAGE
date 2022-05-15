/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

import HelperUtils.*
import cats.effect.{ExitCode, IO, IOApp}
import com.typesafe.scalalogging.Logger

object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _      <- if args.length > 1 then IO.raiseError(new IllegalArgumentException("Need the path to a SLAN spec or no command line parameters")) else IO.unit
      slanSpec <- IO {if args.length == 1 then Some(args.head) else None}
      _     <- IO(if slanSpec == None then println("nothing") else println(s"the input is ${slanSpec.get}"))
    } yield ExitCode.Success


class Main