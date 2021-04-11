import java.net.URL
import java.util
/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

import java.util.HashMap

object LoadExecuteExternalJar extends App {

  import java.net.URLClassLoader

  import java.net.URLClassLoader

  val child = new URLClassLoader(Array(new URL("file:///media/drmark/DELORO/ExampleJar4Experiments/out/artifacts/ExampleJar4Experiments_jar/ExampleJar4Experiments.jar")), this.getClass.getClassLoader)
  //  val child = new URLClassLoader(Array[Nothing](myJar.toURI.toURL), this.getClass.getClassLoader)
  val classToLoad = Class.forName("com.lsc.Transformer", true, child)
  val method = classToLoad.getDeclaredMethod("xform", classOf[HashMap[Integer, Object]])
  val param = new util.HashMap[String, Object]()
  param.put("Mark", 1.asInstanceOf[Integer])
  param.put("Tinka", 2.asInstanceOf[Integer])
  //  val instance = classToLoad.newInstance
  val result = method.invoke(classToLoad.newInstance, param)
  println(result)
}
