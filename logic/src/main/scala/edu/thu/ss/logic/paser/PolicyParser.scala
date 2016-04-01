package edu.thu.ss.logic.paser

import edu.thu.ss.logic.analysis._
import edu.thu.ss.logic.analysis.DefinitionResolver
import edu.thu.ss.logic.analysis.FormulaExpander
import edu.thu.ss.logic.formula.LogicDefinitions
import edu.thu.ss.logic.policy.Policy
import edu.thu.ss.logic.util.Logging
import edu.thu.ss.logic.formula.NamedFormula

/**
 * a high-level parser to handle input file.
 */
class PolicyParser extends Logging {

  def parsePolicy(path: String): Policy = {
    val (definitionList, ruleList) = LogicParser.parseFile(path)

    val definitions: LogicDefinitions = analyzeDefinitions(definitionList)
    parseFormulas(ruleList, definitions)

    new Policy(definitions, ruleList)
  }

  private def analyzeDefinitions(list: Seq[UnresolvedDefinition]): LogicDefinitions = {
    val analyzers: Seq[DefinitionAnalyzer] =
      CheckDefinitionUnique() ::
        DefinitionResolver() ::
        CheckClassCompatibility() ::
        Nil

    val definitions = new LogicDefinitions
    analyzers.foreach(_(list, definitions))
    definitions
  }

  private def parseFormulas(list: Seq[NamedFormula], definitions: LogicDefinitions) {
    val analyzers: Seq[FormulaAnalyzer] =
      FormulaExpander() ::
        CheckFormulaUnique() ::
        FormulaResolver() ::
        FormulaSimplifier() ::
        CheckDecidability() ::
        Nil
    analyzers.foreach(_(list, definitions))
  }

}