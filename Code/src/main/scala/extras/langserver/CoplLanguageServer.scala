package extras.langserver

import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.{LanguageClient, LanguageServer, TextDocumentService, WorkspaceService}
import org.eclipse.lsp4j.{CompletionOptions, Diagnostic, InitializeParams, InitializeResult, MessageParams, MessageType, PublishDiagnosticsParams, SemanticTokensLegend, SemanticTokensWithRegistrationOptions, ServerCapabilities, TextDocumentSyncKind}

import java.io.{InputStream, OutputStream}
import java.util.concurrent.CompletableFuture
import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.language.unsafeNulls

object SemanticTokensLegend {
  def getLegend: SemanticTokensLegend = {
    val tokenTypes = List(
      "type",
      "variable",
      "operator",
      "number",
      "string",
      "comment",
      "function",
      "keyword",
      "namespace",
      "class",
      "enum",
      "interface",
      "struct",
      "typeParameter",
      "parameter",
      "property",
      "enumMember",
      "event",
      "method",
      "macro",
      "regexp",
      "decorator"
    )
    val tokenModifiers = List(
      "declaration",
      "definition",
      "readonly",
      "static",
      "deprecated",
      "abstract",
      "async",
      "modification",
      "documentation",
      "defaultLibrary"
    )
    new SemanticTokensLegend(tokenTypes.asJava, tokenModifiers.asJava)
  }
}

class CoplLanguageServer extends LanguageServer {
  private val textDocumentService = new CoplTextDocumentService(this)
  private val workspaceService    = new CoplWorkspaceService()

  private val openDocuments = mutable.Map[String, String]()

  // Add a field to store the client
  private var client: LanguageClient = uninitialized

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    val capabilities = new ServerCapabilities()

    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    val completionOptions = new CompletionOptions()
    completionOptions.setResolveProvider(false) // Set to true if server provides a `resolveCompletionItem` method
    capabilities.setCompletionProvider(completionOptions)

    val semanticTokensOptions = new SemanticTokensWithRegistrationOptions()
    semanticTokensOptions.setLegend(SemanticTokensLegend.getLegend)
    // semanticTokensOptions.setRange(true)
    semanticTokensOptions.setFull(true)

    capabilities.setSemanticTokensProvider(semanticTokensOptions)

    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  override def getTextDocumentService: TextDocumentService = textDocumentService

  override def getWorkspaceService: WorkspaceService = workspaceService

  override def exit(): Unit = ()

  override def shutdown(): CompletableFuture[Object] =
    CompletableFuture.completedFuture(new Object())

  def connect(client: LanguageClient): Unit =
    this.client = client

  def logInfo(message: String): Unit = {
    val messageParams = new MessageParams()
    messageParams.setType(MessageType.Info)
    messageParams.setMessage(message)
    client.logMessage(messageParams)
  }

  def getOpenDocuments: mutable.Map[String, String] = openDocuments

  def publishDiagnostics(uri: String, diagnostics: List[Diagnostic]): Unit = {
    val params = new PublishDiagnosticsParams()
    params.setUri(uri)
    params.setDiagnostics(diagnostics.asJava)
    client.publishDiagnostics(params)
  }

  def clearDiagnostic(uri: String): Unit = {
    val params = new PublishDiagnosticsParams()
    params.setUri(uri)
    params.setDiagnostics(List().asJava)
    client.publishDiagnostics(params)
  }
}

object CoplLanguageServer {
  def main(args: Array[String]): Unit = {
    // Get the standard input and output streams
    val in: InputStream   = System.in
    val out: OutputStream = System.out

    // Create an instance of the language server
    val server = new CoplLanguageServer()

    // create a new launcher for the language server
    val launcher = LSPLauncher.createServerLauncher(server, in, out)

    // Get the client from the launcher
    val client: LanguageClient = launcher.getRemoteProxy

    // Connect your language server to the client
    server.connect(client)

    // Start listening for requests from the client
    val listening = launcher.startListening()

    // Block on the Future returned by startListening to keep the server running
    listening.get()
  }
}
