package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlTypes}
import Translator.SlanKeywords.Eventual
import Translator.SlantParser.convertJ2S

class GroupResourcesProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case cv: String => List(ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup(Eventual, cv)), SlanValue(convertJ2S(v._2).asInstanceOf)))
      case compositeKey: (Map[_, _] | List[_]) => List(ResourceReferenceInGroup((new GroupResourceConsistencyModelKeyProcessor()).commandProcessor(compositeKey), SlanValue(convertJ2S(v._2).asInstanceOf)))
      case compositeKey: (_, _) => List(ResourceReferenceInGroup((new GroupResourceConsistencyModelKeyProcessor()).commandProcessor(compositeKey), (new GroupResourceReplicationCoeffProcessor()).commandProcessor(convertJ2S(v._2)).asInstanceOf))
      case unknown => (new GroupResourcesProcessor).commandProcessor(convertJ2S(v._1)).asInstanceOf
    }

    case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
  }
}