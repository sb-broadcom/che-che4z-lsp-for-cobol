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
package org.eclipse.lsp.cobol.extendedapi;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.ToNumberPolicy;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.lsp.cobol.cfg.CFASTBuilder;
import org.eclipse.lsp.cobol.cfg.CFASTBuilderImpl;
import org.eclipse.lsp.cobol.common.AnalysisResult;
import org.eclipse.lsp.cobol.service.DocumentModelService;
import org.eclipse.lsp.cobol.test.engine.UseCase;
import org.eclipse.lsp.cobol.test.engine.UseCaseUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/** Test for @link({@link CFASTBuilderImpl}. */
@Slf4j
class CFASTBuilderTest {
  private static final Gson GSON =
      new GsonBuilder()
          .setPrettyPrinting()
          .setObjectToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE)
          .create();

  static Stream<Arguments> casesToTest() throws IOException {
    return Files.list(Paths.get("src", "test", "resources", "cfast"))
        .filter(p -> p.toString().endsWith(".cbl"))
        .map(CFASTBuilderTest::toArguments);
  }

  /**
   * Test that each file is processed into expected CF AST.
   *
   * @param src cobol program.
   * @param jsonTree expected cf tree in json format.
   * @param caseName test name
   */
  @ParameterizedTest(name = "Test CFAST Builder: {2}")
  @MethodSource("casesToTest")
  void cfastBuilderTest(String src, String jsonTree, String caseName) {
    AnalysisResult analysisResult =
        UseCaseUtils.analyze(
            UseCase.builder()
                .documentUri("fake/path")
                .text(src)
                .build());
    DocumentModelService documentModelService = new DocumentModelService();
    documentModelService.openDocument("fake/path", src, "COBOL");
    CFASTBuilder builder = new CFASTBuilderImpl(documentModelService);
    Assertions.assertEquals(
        GSON.toJson(GSON.fromJson(jsonTree, List.class)),
        GSON.toJson(
            GSON.fromJson(
                GSON.toJson(builder.build(analysisResult.getRootNode().findFirstProgramNode()).getControlFlowAST()),
                List.class)));
  }

  private static Arguments toArguments(Path p) {
    try {
      final String resultName = p.getFileName().toString().replace(".cbl", ".result.json");
      final String resultContent =
          new String(Files.readAllBytes(p.getParent().resolve(resultName)));
      return Arguments.arguments(
          new String(Files.readAllBytes(p)), resultContent, p.getFileName().toString());
    } catch (IOException e) {
      LOG.error(e.getMessage(), e);
    }
    return null;
  }
}
