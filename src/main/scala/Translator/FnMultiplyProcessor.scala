package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlPrimitiveTypes, YamlTypes}
import Translator.SlanKeywords.{FOREACH, FnPrefix, Fn_Multiply, Fn_Update, IF}
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class FnMultiplyProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case simpleOperand: String => List(SlanValue(simpleOperand))
    case v: (_, _) => (new FunctionProcessor).commandProcessor(convertJ2S(v)).asInstanceOf
    case entry: YamlPrimitiveTypes => List(SlanValue(entry))
    case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
  }
