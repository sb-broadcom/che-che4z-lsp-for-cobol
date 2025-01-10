/*
 * Copyright (c) 2021 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *    Broadcom, Inc. - initial API and implementation
 *
 */
package org.eclipse.lsp.cobol.cfg;

import com.google.gson.Gson;
import com.google.inject.Inject;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.lsp.cobol.common.model.tree.*;
import org.eclipse.lsp.cobol.common.model.tree.statements.StatementNode;
import org.eclipse.lsp.cobol.common.model.tree.variable.VariableUsageNode;
import org.eclipse.lsp.cobol.common.model.variables.DivisionType;
import org.eclipse.lsp.cobol.core.model.extendedapi.*;
import org.eclipse.lsp.cobol.implicitDialects.cics.nodes.ExecCicsHandleNode;
import org.eclipse.lsp.cobol.implicitDialects.cics.nodes.ExecCicsNode;
import org.eclipse.lsp.cobol.implicitDialects.cics.nodes.ExecCicsReturnNode;
import org.eclipse.lsp.cobol.implicitDialects.sql.node.Db2DataAndProcedureDivisionNode;
import org.eclipse.lsp.cobol.implicitDialects.sql.node.ExecSqlNode;
import org.eclipse.lsp.cobol.implicitDialects.sql.node.ExecSqlWheneverNode;
import org.eclipse.lsp.cobol.service.CobolDocumentModel;
import org.eclipse.lsp.cobol.service.DocumentModelService;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;

import java.util.ArrayList;
import java.util.List;

import static org.eclipse.lsp.cobol.common.model.NodeType.*;

/** CF tree builder implementation */
@Slf4j
public class CFASTBuilderImpl implements CFASTBuilder {
  private static final int SNIPPET_LENGTH = 10;

  private final DocumentModelService documentModelService;

  @Inject
  public CFASTBuilderImpl(DocumentModelService documentModelService) {
    this.documentModelService = documentModelService;
  }

  @Override
  public ExtendedApiResult build(ProgramNode programNode) {
    ExtendedApiResult result = new ExtendedApiResult(new ArrayList<>());
    if (programNode == null) {
      return result;
    }
    traverse(programNode, result.getControlFlowAST());
    LOG.debug(new Gson().toJson(result));
    return result;
  }

  private void traverse(CFASTNode parent, Node node) {
    if (node instanceof ParagraphNode) {
      Paragraph paragraph =
          new Paragraph(
              ((ParagraphNode) node).getName(),
              cutSnippet(node),
              convertLocation(node));
      addChild(parent, paragraph);
      node.getChildren().forEach(child -> traverse(paragraph, child));
    } else if (node instanceof ProcedureSectionNode) {
      Section section =
          new Section(
              ((ProcedureSectionNode) node).getName(),
              cutSnippet(node),
              convertLocation(node));
      addChild(parent, section);
      node.getChildren().forEach(child -> traverse(section, child));
    } else if (node instanceof Db2DataAndProcedureDivisionNode) {
      node.getChildren().forEach(child -> traverse(parent, child));
    } else if (node instanceof GoToNode) {
      addChild(parent, new GoTo(((GoToNode) node).getTargets(), convertLocation(node)));
    } else if (node instanceof EvaluateNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.EVALUATE.getValue(), convertLocation(node)));
      node.getChildren().forEach(child -> traverse(parent, child));
      addChild(parent, new CFASTNode(CFASTNodeType.END_EVALUATE.getValue(), convertLocation(node)));
    } else if (node instanceof EvaluateWhenNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.WHEN.getValue(), convertLocation(node)));
    } else if (node instanceof EvaluateWhenOtherNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.WHEN_OTHER.getValue(), convertLocation(node)));
      node.getChildren().forEach(child -> traverse(parent, child));
    } else if (node instanceof IfNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.IF.getValue(), convertLocation(node)));
      node.getChildren().forEach(child -> traverse(parent, child));
      addChild(parent, new CFASTNode(CFASTNodeType.ENDIF.getValue(), convertLocation(node)));
    } else if (node instanceof SentenceNode) {
      node.getChildren().forEach(child -> traverse(parent, child));
    } else if (node instanceof XMLParseNode) {
      XMLParseNode xmlParseNode = (XMLParseNode) node;
      addChild(parent, new XmlParse(xmlParseNode.getProcessingProcedureName(), xmlParseNode.getThruProcedureName(), convertLocation(node)));
      node.getChildren().forEach(child -> traverse(parent, child));
      addChild(parent, new CFASTNode(CFASTNodeType.END_XML.getValue(), convertLocation(node)));
    } else if (node instanceof IfElseNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.ELSE.getValue(), convertLocation(node)));
      node.getChildren().forEach(child -> traverse(parent, child));
    } else if (node instanceof PerformNode) {
      if (((PerformNode) node).isInline()) {
        PerformUntilType performUntilType = getPerformUntilType((PerformNode) node);
        addChild(parent, new InlinePerform(convertLocation(node), performUntilType));
        node.getChildren().forEach(child -> traverse(parent, child));
        addChild(parent, new CFASTNode(CFASTNodeType.END_INLINE_PERFORM.getValue(), convertLocation(node)));
      } else {
        PerformNode performNode = ((PerformNode) node);
        PerformUntilType performUntilType = getPerformUntilType((PerformNode) node);

        addChild(parent,
            new Perform(performNode.getTarget(),
                performNode.getThru(),
                convertLocation(node),
                performUntilType
            ));
      }
    } else if (node instanceof ExitPerformNode) {
      ExitPerformNode exitPerformNode = (ExitPerformNode) node;
      addChild(parent, new ExitPerform(exitPerformNode.isCycle(), exitPerformNode.isInsideInlinePerform(), convertLocation(node)));
    } else if (node instanceof ExitParagraphNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.EXIT_PARAGRAPH.getValue(), convertLocation(node)));
    } else if (node instanceof ExitSectionNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.EXIT_SECTION.getValue(), convertLocation(node)));
    } else if (node instanceof ExitNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.EXIT.getValue(), convertLocation(node)));
    } else if (node instanceof GoBackNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.GOBACK.getValue(), convertLocation(node)));
    } else if (node instanceof ExecCicsNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.EXEC_CICS.getValue(), convertLocation(node)));
      node.getChildren().forEach(child -> traverse(parent, child));
      addChild(parent, new CFASTNode(CFASTNodeType.END_EXEC.getValue(), convertLocation(node)));
    } else if (node instanceof ExecCicsReturnNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.GOBACK.getValue(), convertLocation(node)));
    } else if (node instanceof UseNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.USE.getValue(), convertLocation(node)));
    } else if (node instanceof UseForDebuggingNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.USE_FOR_DEBUGGING.getValue(), convertLocation(node)));
    } else if (node instanceof ExecCicsHandleNode) {
      ExecCicsHandleNode.HandleAbendType type = ((ExecCicsHandleNode) node).getType();
      String value;

      switch (type) {
        case PROGRAM:
          value = node.getDepthFirstStream().filter(n -> n.getNodeType() == VARIABLE_USAGE)
              .findFirst()
              .map(VariableUsageNode.class::cast)
              .map(VariableUsageNode::getName)
              .orElse(null);
          break;
        case LABEL:
          value = node.getDepthFirstStream().filter(n -> n.getNodeType() == CODE_BLOCK_USAGE)
              .findFirst()
              .map(CodeBlockUsageNode.class::cast)
              .map(CodeBlockUsageNode::getName)
              .orElse(null);
          break;
        default:
          value = null;
          break;
      }

      addChild(parent, new HandleAbend(convertLocation(node), type.toString(), value));
      node.getChildren().forEach(child -> traverse(parent, child));
      addChild(parent, new CFASTNode(CFASTNodeType.END_EXEC.getValue(), convertLocation(node)));
    } else if (node instanceof ExecSqlNode) {
      boolean isWhenever = node.getChildren().stream().anyMatch(n ->
              n.getDepthFirstStream().anyMatch(nd -> nd instanceof ExecSqlWheneverNode));
      // CCF expects to have "execwhenever" instead of "execsql" in case of whenever SQL statement.
      if (!isWhenever) {
        addChild(parent, new CFASTNode(CFASTNodeType.EXEC_SQL.getValue(), convertLocation(node)));
      }
      node.getChildren().forEach(child -> traverse(parent, child));
      addChild(parent, new CFASTNode(CFASTNodeType.END_EXEC.getValue(), convertLocation(node)));
    } else if (node instanceof ExecSqlWheneverNode) {
      ExecSqlWheneverNode wheneverNode = (ExecSqlWheneverNode) node;
      SqlWhenever cfastNode = new SqlWhenever(convertLocation(node),
          wheneverNode.getWheneverConditionType().name(),
          wheneverNode.getWheneverType().name(),
          wheneverNode.getValue());

      addChild(parent, cfastNode);
      node.getChildren().forEach(child -> traverse(parent, child));
    } else if (node instanceof StopNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.STOP.getValue(), convertLocation(node)));
    } else if (node instanceof ParagraphsNode || node instanceof ProcedureDivisionBodyNode) {
      node.getChildren().forEach(child -> traverse(parent, child));
    } else if (node instanceof AtEndNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.AT_END.getValue(), convertLocation(node)));
      node.getChildren().forEach(child -> traverse(parent, child));
      addChild(parent, new CFASTNode(CFASTNodeType.AT_END_EXIT.getValue(), convertLocation(node)));
    } else if (node instanceof MergeNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.MERGE.getValue(), convertLocation(node)));
      node.getChildren().forEach(child -> traverse(parent, child));
      addChild(parent, new CFASTNode(CFASTNodeType.END_MERGE.getValue(), convertLocation(node)));
    } else if (node instanceof SortNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.SORT.getValue(), convertLocation(node)));
      node.getChildren().forEach(child -> traverse(parent, child));
      addChild(parent, new CFASTNode(CFASTNodeType.END_SORT.getValue(), convertLocation(node)));
    } else if (node instanceof InputNode) {
      InputNode inputNode = (InputNode) node;
      addChild(parent, new Input(inputNode.getTarget(), inputNode.getThru(), convertLocation(node)));
    } else if (node instanceof OutputNode) {
      OutputNode outputNode = (OutputNode) node;
      addChild(parent, new Output(outputNode.getTarget(), outputNode.getThru(), convertLocation(node)));
    } else if (node instanceof AlterNode) {
      AlterNode alterNode = (AlterNode) node;
      addChild(parent, new Alter(alterNode.getFrom(), alterNode.getTo(), convertLocation(node)));
    } else if (node instanceof OnExceptionNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.ON_EXCEPTION.getValue(), convertLocation(node)));
      node.getChildren().forEach(child -> traverse(parent, child));
      addChild(parent, new CFASTNode(CFASTNodeType.END_ON.getValue(), convertLocation(node)));
    } else if (node instanceof OnNotExceptionNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.ON_NOT_EXCEPTION.getValue(), convertLocation(node)));
      node.getChildren().forEach(child -> traverse(parent, child));
      addChild(parent, new CFASTNode(CFASTNodeType.END_ON.getValue(), convertLocation(node)));
    } else if (node instanceof StatementNode) {
      addChild(parent, new CFASTNode(CFASTNodeType.STATEMENT.getValue(), convertLocation(node)));
      node.getChildren().forEach(child -> traverse(parent, child));
    }
  }

  private void traverse(ProgramNode node, List<Program> programs) {
    node.getChildren().stream()
        .filter(it -> it instanceof DivisionNode)
        .map(DivisionNode.class::cast)
        .filter(it -> it.getDivisionType() == DivisionType.PROCEDURE_DIVISION)
        .findFirst()
        .ifPresent(
            n -> {
              Program program = new Program(node.getProgramName(), convertLocation(n));
              n.getChildren().forEach(child -> traverse(program, child));
              traverse(program, n);
              programs.add(program);
            });
  }

  private void addChild(CFASTNode target, CFASTNode child) {
    if (target.getChildren() == null) {
      target.setChildren(new ArrayList<>());
    }
    target.getChildren().add(child);
  }

  private Location convertLocation(Node node) {
    final org.eclipse.lsp4j.Location location = node.getLocality().toLocation();
    final Range range = location.getRange();

    final Position startPosition = new Position();
    startPosition.setLine(range.getStart().getLine() + 1);
    startPosition.setCharacter(range.getStart().getCharacter() + 1);
    final Position endPosition = new Position();
    endPosition.setLine(range.getEnd().getLine() + 1);
    endPosition.setCharacter(range.getEnd().getCharacter() + 1);

    return new Location(location.getUri(), startPosition, endPosition);
  }

  private String cutSnippet(Node node) {
    CobolDocumentModel doc = documentModelService.get(node.getLocality().getUri());
    if (doc == null) {
      LOG.error("cutSnippet failed: " + node.getLocality().getUri() + " not found.");
      return "<snippet creation error>";
    }
    List<CobolDocumentModel.Line> lines = doc.getLines();
    StringBuilder sb = new StringBuilder();
    int startLine = node.getLocality().getRange().getStart().getLine();
    int stopLine = Math.min(startLine + SNIPPET_LENGTH, node.getLocality().getRange().getEnd().getLine() + 1);
    lines.subList(startLine, stopLine).forEach(line -> {
      if (sb.length() > 0) {
        sb.append("\r\n");
      }
      sb.append(line.getText());
    });
    return sb.toString();
  }

  private PerformUntilType getPerformUntilType(PerformNode performNode) {
    return performNode.getDepthFirstStream()
        .filter(n -> n instanceof PerformUntilNode)
        .findFirst()
        .map(PerformUntilNode.class::cast)
        .map(PerformUntilNode::isUntilExit)
        .map(n -> n ? PerformUntilType.UNTIL_EXIT : PerformUntilType.UNTIL_CONDITION)
        .orElse(null);
  }
}
