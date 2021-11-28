package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlTypes}
import Translator.SlanKeywords.Eventual
import Translator.SlantParser.convertJ2S

class BehaviorMessageKeyProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case cv: String => List(SlanValue(cv))
    case v: (_, _) => convertJ2S(v._1) match {
      case compositeKey: List[_] =>
        val msgIds = for {
          msgId <- compositeKey
        } yield SlanValue(convertJ2S(msgId).toString)
        List(MessageResponseBehavior(msgIds, (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }
    case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
  }
}
