package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlTypes}
import Translator.SlantParser.convertJ2S
import Translator.SlanKeywords.*
import cats.implicits.*
import cats.kernel.Eq

class GroupProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case cv: String if cv.toUpperCase === Resources.toUpperCase => (new GroupResourcesProcessor).commandProcessor(convertJ2S(v._2))
      case cv: String => List(GroupAgent(cv, (new GroupAgentsProcessor).commandProcessor(convertJ2S(v._2)).asInstanceOf))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }

    case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
  }
}
