package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlTypes}
import Translator.SlanKeywords.{ExternalService, FOREACH, FnPrefix, IF, NOT}
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class BehaviorActionsProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case entry: String if entry.toUpperCase === NOT.toUpperCase => List(Not((new NotProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === IF.toUpperCase => List(IfThenElse((new IfThenElseProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === FOREACH.toUpperCase => (new CollectionIteratorProcessor).commandProcessor(convertJ2S(v._2))
      case entry: String if entry.toUpperCase === ExternalService.toUpperCase => (new ExternalServiceDefinitionProcessor).commandProcessor(convertJ2S(v._2))
      case entry: String if entry.toUpperCase.startsWith(FnPrefix.toUpperCase) => (new FunctionProcessor).commandProcessor(convertJ2S(v))
      //some custom external function
      case entry: String => (new FunctionProcessor).commandProcessor(convertJ2S(v))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }

    case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
  }
