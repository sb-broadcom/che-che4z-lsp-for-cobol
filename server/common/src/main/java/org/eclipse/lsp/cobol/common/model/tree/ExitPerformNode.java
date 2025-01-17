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

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.eclipse.lsp.cobol.common.model.Locality;
import org.eclipse.lsp.cobol.common.model.NodeType;

/** The class represents exit paragraph in COBOL. */
@Getter
@ToString(callSuper = true)
public class ExitPerformNode extends Node {
  private final boolean cycle;
  @Setter
  private boolean insideInlinePerform;

  public ExitPerformNode(Locality location, boolean cycle) {
    super(location, NodeType.EXIT_PERFORM);
    this.cycle = cycle;
  }
}
