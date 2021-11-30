package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlPrimitiveTypes, YamlTypes}
import Translator.SlantParser.convertJ2S
import SlanKeywords.*
import cats.Eq
import cats.implicits.*

class NotProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case entry: String if entry.toUpperCase === NOT.toUpperCase => List(Not((new NotProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === AND.toUpperCase || entry.toUpperCase === OR.toUpperCase || entry.toUpperCase === LessThen.toUpperCase ||
        entry.toUpperCase === LessEqual.toUpperCase || entry.toUpperCase === GreaterThen.toUpperCase ||
        entry.toUpperCase === GreaterEqual.toUpperCase || entry.toUpperCase === EqualTo.toUpperCase => List( (new RelOpProcessor).commandProcessor(convertJ2S(v._2)).asInstanceOf)
      case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
    }

    case entry: YamlPrimitiveTypes => List(SlanValue(entry))

    case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
  }
