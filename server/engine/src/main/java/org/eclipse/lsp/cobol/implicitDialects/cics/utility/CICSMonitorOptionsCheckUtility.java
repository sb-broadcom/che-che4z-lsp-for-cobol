/*
 * Copyright (c) 2024 Broadcom.
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
package org.eclipse.lsp.cobol.implicitDialects.cics.utility;

import org.antlr.v4.runtime.ParserRuleContext;
import org.eclipse.lsp.cobol.common.dialects.DialectProcessingContext;
import org.eclipse.lsp.cobol.common.error.ErrorSeverity;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.eclipse.lsp.cobol.implicitDialects.cics.CICSLexer;
import org.eclipse.lsp.cobol.implicitDialects.cics.CICSParser;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.eclipse.lsp.cobol.implicitDialects.cics.CICSParser.RULE_cics_monitor;

/** Checks CICS MONITOR rules for required and invalid options */
public class CICSMonitorOptionsCheckUtility extends CICSOptionsCheckBaseUtility {
  public static final int RULE_INDEX = RULE_cics_monitor;

  private static final Map<Integer, ErrorSeverity> DUPLICATE_CHECK_OPTIONS =
          new HashMap<Integer, ErrorSeverity>() {
            {
              put(CICSLexer.POINT, ErrorSeverity.ERROR);
              put(CICSLexer.DATA1, ErrorSeverity.ERROR);
              put(CICSLexer.DATA2, ErrorSeverity.ERROR);
              put(CICSLexer.ENTRYNAME, ErrorSeverity.ERROR);
            }
          };

  public CICSMonitorOptionsCheckUtility(DialectProcessingContext context, List<SyntaxError> errors) {
    super(context, errors, DUPLICATE_CHECK_OPTIONS);
  }

  /**
   * Entrypoint to check CICS MONITOR rule options
   *
   * @param ctx ParserRuleContext subclass containing options
   * @param <E> A subclass of ParserRuleContext
   */
  public <E extends ParserRuleContext> void checkOptions(E ctx) {
    if (ctx instanceof CICSParser.Cics_monitor_optionsContext) {
      checkMonitor((CICSParser.Cics_monitor_optionsContext) ctx);
    }
    checkDuplicates(ctx);
  }

  private void checkMonitor(CICSParser.Cics_monitor_optionsContext ctx) {
    checkHasMandatoryOptions(ctx.POINT(), ctx, "POINT");
  }
}