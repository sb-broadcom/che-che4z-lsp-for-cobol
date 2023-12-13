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
package org.eclipse.lsp.cobol.lsp.handlers.text;

import static java.lang.String.format;

import com.google.inject.Inject;
import java.util.Objects;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.lsp.cobol.common.copybook.CopybookService;
import org.eclipse.lsp.cobol.lsp.AsyncAnalysisService;
import org.eclipse.lsp.cobol.lsp.DisposableLSPStateService;
import org.eclipse.lsp.cobol.service.DocumentModelService;
import org.eclipse.lsp.cobol.service.WatcherService;
import org.eclipse.lsp.cobol.service.copybooks.CopybookServiceImpl;
import org.eclipse.lsp.cobol.service.utils.UriHelper;
import org.eclipse.lsp4j.DidCloseTextDocumentParams;

/**
 * LSP DidClose Handler
 */
@Slf4j
public class DidCloseHandler {
  private final DisposableLSPStateService disposableLSPStateService;
  private final AsyncAnalysisService asyncAnalysisService;
  private final DocumentModelService documentModelService;
  private final WatcherService watcherService;
  private final CopybookService copybookService;

  @Inject
  public DidCloseHandler(DisposableLSPStateService disposableLSPStateService,
                         AsyncAnalysisService asyncAnalysisService, DocumentModelService documentModelService, WatcherService watcherService, CopybookService copybookService) {
    this.disposableLSPStateService = disposableLSPStateService;
    this.asyncAnalysisService = asyncAnalysisService;
    this.documentModelService = documentModelService;
    this.watcherService = watcherService;
    this.copybookService = copybookService;
  }

  /**
   * Handle LSP didClose request.
   * @param params DidCloseTextDocumentParams.
   */
  public void didClose(DidCloseTextDocumentParams params) throws InterruptedException {
    if (disposableLSPStateService.isServerShutdown()) {
      return;
    }
    String uri = UriHelper.decode(params.getTextDocument().getUri());
    LOG.info(format("Document closing invoked on URI %s", uri));
    watcherService.removeRuntimeWatchers(uri);
    documentModelService.closeDocument(uri);
    if (copybookService instanceof CopybookServiceImpl) {
      CopybookServiceImpl copybookServiceImpl = (CopybookServiceImpl) copybookService;
      copybookServiceImpl.getCopybookUsage(uri).stream()
          .filter(copybookModel -> Objects.isNull(copybookModel.getContent()))
          .forEach(
              copybookModel -> copybookServiceImpl.invalidateCache(copybookModel.getCopybookId()));
    }
    asyncAnalysisService.cancelAnalysis(uri);
  }
}