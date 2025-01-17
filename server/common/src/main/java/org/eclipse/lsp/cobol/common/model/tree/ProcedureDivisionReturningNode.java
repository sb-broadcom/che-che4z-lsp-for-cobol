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
package org.eclipse.lsp.cobol.common.model.tree;

import lombok.ToString;
import org.eclipse.lsp.cobol.common.model.Locality;
import org.eclipse.lsp.cobol.common.model.NodeType;
import org.eclipse.lsp.cobol.common.model.tree.variable.VariableNameAndLocality;

@ToString(callSuper = true)
public class ProcedureDivisionReturningNode extends Node {
  public final VariableNameAndLocality variable;

  public ProcedureDivisionReturningNode(Locality location, VariableNameAndLocality v) {
    super(location, NodeType.PROCEDURE_RETURNING);
    this.variable = v;
  }
}
