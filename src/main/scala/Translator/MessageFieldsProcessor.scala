package Translator

import Translator.SlanAbstractions.{SlanConstruct, YamlPrimitiveTypes, YamlTypes}
import Translator.SlantParser.convertJ2S

class MessageFieldsProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: List[_] => v.map(aV => SlanValue(convertJ2S(aV).toString))
    case v: (_, _) => (new ResourcesProcessor).commandProcessor(convertJ2S(v))
    case simpleValue: YamlPrimitiveTypes => List(SlanValue(simpleValue))
    case None => List()
    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
