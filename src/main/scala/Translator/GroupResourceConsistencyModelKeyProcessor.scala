package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlTypes}
import Translator.SlanKeywords.Eventual
import Translator.SlantParser.convertJ2S

class GroupResourceConsistencyModelKeyProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case cv: String => List(ResourceConsistencyModelInGroup(Eventual, cv))
    case v: (_, _) => convertJ2S(v._1) match {
      case cv: String => List(ResourceConsistencyModelInGroup(cv, convertJ2S(v._2).toString))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }

    case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
  }
}

