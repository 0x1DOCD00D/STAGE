/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import Translator.SlanAbstractions.YamlTypes
import Translator.SlanConstruct.IncorrectYamlType
import Translator.SlanKeywords.{Agents, Behavior, Models}
import cats.effect.IO
import cats.effect.kernel.Outcome.{Errored, Succeeded}
import cats.effect.testing.scalatest.AsyncIOSpec
import org.joda.time.DateTime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import java.util.Date
import scala.concurrent.Future
import scala.io.Source
import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}

class SlantParserTest extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  val complexYamlTemplate_2 = "SlanFeatureTesting/ComplexKey_v2.yaml"
  val complexYamlTemplate_1 = "SlanFeatureTesting/ComplexKey_v1.yaml"
  val basicYamlTemplate = "SlanFeatureTesting/Template_v1.yaml"
  val illFormedYaml = "SlanFeatureTesting/IllFormed.yaml"
  val nonexistentYamlFile = "Hades.yaml"
  val singleScalarFloatingPointValueFile = "SlanFeatureTesting/OneScalarFloatingPointValue.yaml"
  val singleScalarDateValueFile = "SlanFeatureTesting/OneScalarDateValue.yaml"
  val singleScalarIntValueFile = "SlanFeatureTesting/OneScalarIntValue.yaml"
  val singleScalarStringValueFile = "SlanFeatureTesting/OneScalarStringValue.yaml"
  val singleScalarBooleanValueFile = "SlanFeatureTesting/OneScalarBooleanValue.yaml"
  val singleScalarNullValueFile = "SlanFeatureTesting/OneScalarNULLValue.yaml"
  val blockSequenceFile = "SlanFeatureTesting/BlockSequenceSimple.yaml"
  val stringScalarValue = "just one string value"
  val intScalarValue = 1234567
  val floatScalarValue = 123450.6789
  val boolScalarValue = false

  def parseObtainYamlRef(path: String): IO[YamlTypes] = {
    for {
      ymlModelFib <- SlantParser(path).start
      ymlModel <- ymlModelFib.join
      ymlIo = ymlModel match
        case Succeeded(result) => for {
          fa <- result
          outyml = fa match
            case Right(SlanYamlHandle(ymlref)) => ymlref
            case err => IO.raiseError(new RuntimeException(s"Incorrect value computed: ${err.toString}"))
        } yield outyml
        case Errored(e) => IO.raiseError(e)
        case _ => IO.raiseError(new RuntimeException("Fiber computation failure."))
      yml <- ymlIo
      reseval = SlantParser.convertJ2S(yml)
    } yield reseval
  }

  "Loading and Parsing Slan Programs" - {
    "issue an error if the file does not exist" in {
      val errMsg = for {
        parsed <- SlantParser(nonexistentYamlFile)
        failed = parsed match
          case Left(FileRuntimeError(e)) => e.getMessage
          case err => IO.raiseError(new RuntimeException(s"Incorrect value computed: ${err.toString}"))
      } yield failed
      errMsg.asserting(_ shouldBe "Hades.yaml (No such file or directory)")
    }

    "load up and extract the content of the single null entry in yaml" in {
      val path = getClass.getClassLoader.getResource(singleScalarNullValueFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: Option[_] => None
          case err => Some(err)
        }
      } yield res

      yr.asserting(_ shouldBe None)
    }

    "load up and extract the content of the single scalar string value incorrectly treated as boolean" in {
      val path = getClass.getClassLoader.getResource(singleScalarStringValueFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: Boolean => v
          case _ => None
        }
      } yield res

      yr.asserting(_ shouldBe None)
    }

    "load up and extract the content of the single scalar boolean value" in {
      val path = getClass.getClassLoader.getResource(singleScalarBooleanValueFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: Boolean => v
          case _ => !boolScalarValue
        }
      } yield res

      yr.asserting(_ shouldBe boolScalarValue)
    }

    "load up and extract the content of the single scalar date value" in {
      val wrongDate = new DateTime(new Date())
      val oracle = DateTime.parse("2021-11-06T00:00:00.000-05:00")
      val path = getClass.getClassLoader.getResource(singleScalarDateValueFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: String => DateTime.parse(v)
          case v: DateTime => v
          case _ => wrongDate
        }
      } yield res

      yr.asserting(_.toString shouldBe oracle.toString())
    }

    "load up and extract the content of the single scalar string value yaml" in {
      val path = getClass.getClassLoader.getResource(singleScalarStringValueFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: String => v
          case _ => null
        }
      } yield res

      yr.asserting(_ shouldBe stringScalarValue)
    }

    "load up and extract the content of the single scalar floating point value yaml" in {
      val path = getClass.getClassLoader.getResource(singleScalarFloatingPointValueFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: Double => v
          case _ => -floatScalarValue
        }
      } yield res

      yr.asserting(_ shouldBe floatScalarValue)
    }

    "load up and extract the content of the single scalar int value yaml" in {
      val path = getClass.getClassLoader.getResource(singleScalarIntValueFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: Int => v
          case _ => -intScalarValue
        }
      } yield res

      yr.asserting(_ shouldBe intScalarValue)
    }

    "throw an exception for ill-formed yaml" in {
      val path = getClass.getClassLoader.getResource(illFormedYaml).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: IncorrectYamlType => v.typeName
          case err => err.getClass().getName
        }
      } yield res

      yr.asserting(_ shouldBe "The following type is not handled in the Yaml script: cats.effect.IO$Error")
    }
  }

  "the Slant parser for lists of values" - {
    val seqOfScalarsFile = "SlanFeatureTesting/SeqOfScalars.yaml"
    val seqFlowOfScalarsFile = "SlanFeatureTesting/SeqFlowScalars.yaml"
    val seqOfSeqOfScalarsFile = "SlanFeatureTesting/SeqOfSeqOfScalars.yaml"

    "load up and extract the content of the single scalar int value yaml" in {
      val path = getClass.getClassLoader.getResource(seqOfScalarsFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: List[_] => v.asInstanceOf[List[_]]
          case _ => null
        }
      } yield res

      yr.asserting(_ shouldBe List(stringScalarValue, intScalarValue, floatScalarValue, boolScalarValue, null))
    }

    "load up and extract the content of the flow sequence of scalars from a yaml file" in {
      val path = getClass.getClassLoader.getResource(seqFlowOfScalarsFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: List[_] => v.asInstanceOf[List[_]]
          case _ => null
        }
      } yield res

      yr.asserting(_ shouldBe List(stringScalarValue, intScalarValue, floatScalarValue, boolScalarValue, null))
    }

    "load up and extract the content of the sequence of flow sequences of scalars from a yaml file" in {
      val path = getClass.getClassLoader.getResource(seqOfSeqOfScalarsFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: List[_] => v.asInstanceOf[List[_]]
          case _ => List()
        }
      } yield res

      yr.asserting(_ should not be List())
      yr.asserting(_.length shouldBe 3)
    }
  }

  "the Slant parser for complex yaml scripts" - {
    val simpleMapFile = "SlanFeatureTesting/OneSimpleMap.yaml"
    val keyName = "key"
    val keyValue = "value"
    val threeSimpleMapsFile = "SlanFeatureTesting/ThreeSimpleMaps.yaml"

    "load up and extract the content of one simple map from a yaml file" in {
      val path = getClass.getClassLoader.getResource(simpleMapFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: Map[_, _] => v.asInstanceOf[Map[String, String]]
          case _ => Map()
        }
      } yield res

      yr.asserting(_ should not be null)
      yr.asserting(_.get(keyName) shouldBe Option(keyValue))
    }

    "load up and extract the content of three simple maps from a yaml file" in {
      val path = getClass.getClassLoader.getResource(threeSimpleMapsFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: Map[_, _] => v.asInstanceOf[Map[String, String]]
          case _ => Map()
        }
      } yield res

      yr.asserting(_.toList shouldBe List(("key1","value1"), ("key2","value2"), ("key3","value3")))
    }

    "load up and extract the content of block sequences with dashes from a script file" in {
      val path = getClass.getClassLoader.getResource(blockSequenceFile).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: List[_] => v
          case _ => List()
        }
      } yield res

      yr.asserting(_.length shouldBe 4)
    }

    "extract the content of a complex key as a list of entries" in {
      val key = "[Key entry 1, Key entry 2]"
      val path = getClass.getClassLoader.getResource(complexYamlTemplate_1).getPath
      val yr = for {
        yamlRefIo <- parseObtainYamlRef(path)
        res = yamlRefIo match {
          case v: Map[_,_] => v.asInstanceOf[Map[_,_]].toList
          case _ => List()
        }
      } yield res

      yr.asserting(_.head._1.toString shouldBe key)
    }
  }
}