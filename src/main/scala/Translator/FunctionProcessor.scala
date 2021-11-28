package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlTypes}
import Translator.SlanKeywords.{FOREACH, FnPrefix, Fn_Multiply, Fn_Update, IF}
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class FunctionProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case entry: String if entry.toUpperCase === Fn_Update.toUpperCase => List(FnUpdate((new FnUpdateProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === Fn_Multiply.toUpperCase => List(FnMultiply((new FnMultiplyProcessor).commandProcessor(convertJ2S(v._2))))
      case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
    }

    case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
  }
