package extras.langserver

import org.eclipse.lsp4j.services.WorkspaceService
import org.eclipse.lsp4j.{DidChangeConfigurationParams, DidChangeWatchedFilesParams}

class CoplWorkspaceService extends WorkspaceService {
  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = {
    // TODO: Handle configuration change event here.
  }

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = {
    // TODO: Handle watched files change event here.
  }
}
