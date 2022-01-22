/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package ExternalSources

import com.google.common.collect.{HashBasedTable, Table}

import java.io.{File, FileInputStream}
import java.nio.channels.FileChannel

object CsvTableLoader:
  def apply(fileName: String, header: Boolean): CsvTableLoader =
    val cvsTableData = new CsvTableLoader(fileName, header)
    cvsTableData

class CsvTableLoader private (val fileName:String, val header:Boolean):
  require(fileName != null)

  def this(fileName: String) = this(fileName, false)

  override def toString: String = s"File $fileName has rows and columns and ".concat(if header then "predefine headers" else "no defined header")

  def loadCsvApkMetadata: List[String] =
    import scala.io.Source
    val fileRef = new File(fileName)
    if fileRef.exists() then Source.fromInputStream(getMMappedInputStream(fileRef)).getLines().toList
    else throw new Exception("File " + fileName)

  private def getMMappedInputStream(file: File): CsvInputStream = {
    val buffer = new FileInputStream(file).getChannel.map(FileChannel.MapMode.READ_ONLY, 0, file.length())
    new CsvInputStream(buffer)
  }

  def parseCSVLine(line: String): List[Option[Double]] = line.split(",").map(_.trim.toDoubleOption).toList

  val weightedGraph: Table[Int, Int, Double] = HashBasedTable.create
  weightedGraph.put(0, 0, 1)
  weightedGraph.put(0, 1, 2)
  weightedGraph.put(0, 2, 3)
  weightedGraph.put(1, 0, 4)
  weightedGraph.put(1, 1, 5)
  weightedGraph.put(1, 2, 6)
  weightedGraph.put(2, 0, 7)
  weightedGraph.put(2, 1, 8)
  weightedGraph.put(2, 2, 9)

