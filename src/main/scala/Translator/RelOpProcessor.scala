package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlPrimitiveTypes, YamlTypes}
import Translator.SlantParser.convertJ2S
import SlanKeywords.*
import cats.Eq
import cats.implicits.*

class RelOpProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case entry: String if entry.toUpperCase === NOT.toUpperCase => List(Not((new RelOpProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === AND.toUpperCase => List(And((new RelOpProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === OR.toUpperCase => List(Or((new RelOpProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === LessThen.toUpperCase => List(ROPLess((new RelOpProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === LessEqual.toUpperCase => List(ROPLessEqual((new RelOpProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === GreaterEqual.toUpperCase => List(ROPGreaterEqual((new RelOpProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === GreaterThen.toUpperCase => List(ROPGreater((new RelOpProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === EqualTo.toUpperCase => List(ROPEqual((new RelOpProcessor).commandProcessor(convertJ2S(v._2))))
      case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
    }

    case entry: YamlPrimitiveTypes => List(SlanValue(entry))

    case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
  }
