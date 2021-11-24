package Translator

import Translator.SlanAbstractions.{SlanConstruct, YamlPrimitiveTypes, YamlTypes}

class GroupResourceReplicationCoeffProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case cv: YamlPrimitiveTypes => List(SlanValue(cv))
    case None => List()
    case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
  }
}