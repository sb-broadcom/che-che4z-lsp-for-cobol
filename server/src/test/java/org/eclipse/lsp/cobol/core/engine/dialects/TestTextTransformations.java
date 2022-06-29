/*
 * Copyright (c) 2022 Broadcom.
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
package org.eclipse.lsp.cobol.core.engine.dialects;

import org.eclipse.lsp.cobol.core.engine.TextTransformations;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Text Transformation tests
 */
public class TestTextTransformations {
    private static final String TEST = "0: abcd\n1: TEST\n";

    @Test
    void replace() {
        TextTransformations tt = new TextTransformations(TEST, "https://example.com/text1.txt");
        tt.replace(new Range(
                        new Position(1, 3),
                        new Position(1, 7)),
                "1234");
        assertEquals("0: abcd\n1: 1234\n", tt.calculateExtendedText());
    }

    @Test
    void replaceMultiline() {
        TextTransformations tt = new TextTransformations(TEST, "https://example.com/text1.txt");
        tt.replace(new Range(
                        new Position(0, 3),
                        new Position(1, 7)),
                "hi");
        assertEquals("0: hi\n", tt.calculateExtendedText());
    }

    @Test
    void replaceExtend() {
        TextTransformations tt = new TextTransformations(TEST, "https://example.com/text1.txt");
        tt.replace(new Range(
                        new Position(0, 3),
                        new Position(1, 7)),
                "Hello, World!\n123\ntest11");
        assertEquals("0: Hello, World!\n123\ntest11\n", tt.calculateExtendedText());
    }

    @Test
    void replaces() {
        TextTransformations tt = new TextTransformations(TEST, "https://example.com/text1.txt");
        tt.replace(new Range(
                        new Position(1, 3),
                        new Position(1, 7)),
                "123");
        tt.replace(new Range(
                        new Position(0, 3),
                        new Position(0, 7)),
                "4321");
        assertEquals("0: 4321\n1: 123\n", tt.calculateExtendedText());
    }
}
