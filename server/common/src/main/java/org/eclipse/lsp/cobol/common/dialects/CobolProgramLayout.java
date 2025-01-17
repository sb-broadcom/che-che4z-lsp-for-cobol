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
package org.eclipse.lsp.cobol.common.dialects;

import java.util.regex.Pattern;
import lombok.Builder;
import lombok.Getter;

/**
 * This class represents a cobol program line layout <sequence><indicator><area A><area B><comment
 * Area>
 */
@Getter
public class CobolProgramLayout {
  private final Integer sequenceLength;
  private final Integer indicatorLength;

  public int getSourceCodeLength() {
    return sequenceLength + indicatorLength + areaALength + areaBLength;
  }

  public int getMaxLineLength() {
    return getSourceCodeLength() + commentAreaLength;
  }

  private final Integer areaALength;
  private final Integer areaBLength;
  private final Integer commentAreaLength;

  private final Pattern cobolLinePattern;

  @Builder
  public CobolProgramLayout(
      Integer sequenceLength,
      Integer indicatorLength,
      Integer areaALength,
      Integer areaBLength,
      Integer commentAreaLength) {
    this.sequenceLength = sequenceLength;
    this.indicatorLength = indicatorLength;
    this.areaALength = areaALength;
    this.areaBLength = areaBLength;
    this.commentAreaLength = commentAreaLength;
    this.cobolLinePattern = Pattern.compile(
        String.format(
            "^(?<sequence>.{0,%s})(?<indicator>.{0,%s})(?<contentA>.{0,%s})(?<contentB>.{0,%s})(?<comment>.{0,%s})(?<extra>.*)$",
            sequenceLength, indicatorLength, areaALength, areaBLength, commentAreaLength));
  }
}
