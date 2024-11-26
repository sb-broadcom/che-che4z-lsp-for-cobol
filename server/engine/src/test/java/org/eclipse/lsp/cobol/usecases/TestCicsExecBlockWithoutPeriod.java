/*
 * Copyright (c) 2023 Broadcom.
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
package org.eclipse.lsp.cobol.usecases;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.eclipse.lsp.cobol.common.error.ErrorSource;
import org.eclipse.lsp.cobol.test.engine.UseCaseEngine;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.eclipse.lsp4j.Range;
import org.junit.jupiter.api.Test;

/** Tests that a mandatory period is required for an exec block */
public class TestCicsExecBlockWithoutPeriod {
  public static final String TEXT2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID.    TEST1.\n"
          + "       ENVIRONMENT DIVISION.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
          + "       01  {$*WS-TIME}                     PIC S9(08) COMP VALUE +0.\n"
          + "       PROCEDURE DIVISION.\n"
          + "           EXEC CICS ASKTIME\n"
          + "                     ABSTIME({$WS-TIME})\n"
          + "           END-EXEC{|1}\n";

  @Test
  void testEndExecBlockRequiredForExecCics() {
    UseCaseEngine.runTest(
        TEXT2,
        ImmutableList.of(),
        ImmutableMap.of(
            "1",
            new Diagnostic(
                new Range(),
                "Missing token . for the CICS EXEC block",
                DiagnosticSeverity.Error,
                ErrorSource.PARSING.getText())
            ));
  }
}
