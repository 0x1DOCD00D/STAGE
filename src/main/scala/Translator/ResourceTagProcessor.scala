package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlTypes}
import Translator.SlantParser.convertJ2S
import Translator.SlanKeywords.*
import cats.implicits.*
import cats.kernel.Eq

class ResourceTagProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case resourceId: String => List(ResourceTag(resourceId, None))
    case v: (_, _) => convertJ2S(v._1) match {
      case storageType: String => convertJ2S(v._2) match {
        case resourceId: String => List(ResourceTag(resourceId, Some(storageType)))
        case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
      }
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }

    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
}