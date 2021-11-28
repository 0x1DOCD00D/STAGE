package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlTypes}
import Translator.SlantParser.convertJ2S
import Translator.SlanKeywords.*
import cats.implicits.*
import cats.kernel.Eq

class MessageResponseBehaviorProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) if v._2 == null => convertJ2S(v._1) match {
      //msgName: {blah blah behavior actions}
      case msgId: String => List(MessageResponseBehavior(List(SlanValue(msgId)), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      //? [msg1, msg2, ..., msgN]: {blah blah behavior actions}
      case msgIdList: (Map[_, _] | List[_]) => (new BehaviorMessageKeyProcessor()).commandProcessor(convertJ2S(v._1))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }
    case v: (_, _) if v._2 != null => convertJ2S(v._1) match {
      //msgName: {blah blah behavior actions}
      case msgId: String => List(MessageResponseBehavior(List(SlanValue(msgId)), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      //? [msg1, msg2, ..., msgN]: {blah blah behavior actions}
      case msgIdList: (Map[_, _] | List[_]) => (new BehaviorMessageKeyProcessor()).commandProcessor(convertJ2S(v))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }

    case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
  }
}