/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package ExternalSources

import java.io.InputStream
import java.nio.ByteBuffer

/*
  This implementation is required for NIO memory mapped files.
 */
class CsvInputStream(val buffer: ByteBuffer) extends InputStream:
  override def read(): Int = if buffer.hasRemaining then buffer.get else -1

  override def read(b: Array[Byte], off: Int, len: Int): Int = {
    if buffer.hasRemaining then {
      val len2get = math.min(len, buffer.remaining())
      buffer.get(b, off, math.min(len, len2get))
      len2get
    }
    else -1
  }