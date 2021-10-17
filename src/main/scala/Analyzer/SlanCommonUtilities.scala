/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package Analyzer

import Analyzer.SlanAbstractions.*
import Analyzer.SlantParser.convertJ2S

object SlanCommonUtilities:
  val unknownProcessor: Key2Yaml2Construct = key => unknown =>
    val c = List(UnknownConstruct(key, unknown.getClass().toString, unknown.toString))
    logger.info(c.toString())
    c

  def isContainerizedContent(yamlObj: Any): Boolean = convertJ2S(yamlObj) match {
    case v@(List | Map) => true
    case unknown => false
  }


  def containerContentProcessor(contextProcessor: Key2Yaml2Construct, key: Option[String] = None): Function1[YamlTypes, List[SlanConstruct]] =
    case v: List[_] =>
    val res = v.foldLeft(List[SlanConstruct]())((a, e) => a ::: contextProcessor(key)(convertJ2S(e)))
    logger.info(s"List result: ${res.toString()}")
    res

    case v: Map[_, _] =>
    val res = v.foldLeft(List[SlanConstruct]())((a, e) => a ::: contextProcessor(key)(convertJ2S(e)))
    logger.info(s"Map result: ${res.toString()}")
    res

    case unknown => unknownProcessor(key)(unknown)
