import { App, FileView, ItemView, MarkdownRenderer, TAbstractFile, TFile, Vault, WorkspaceLeaf} from 'obsidian';
import { Elm } from '../src/Main';
import CardBoardPlugin from './main';

const moment = require('moment');

export class CardBoardView extends ItemView {
  private vault: Vault;
  private codeMirror: CodeMirror.Editor;

  constructor(codeMirror: CodeMirror.Editor, app: App, leaf: WorkspaceLeaf) {
    super(leaf);
    this.app = app;
    this.vault = app.vault;
    this.codeMirror = codeMirror;
  }

  async onOpen() {
    // @ts-ignore
    const dailyNotesSettings = this.app.internalPlugins.getPluginById("daily-notes")?.instance?.options;
    dailyNotesSettings.folder = dailyNotesSettings.folder || null
    dailyNotesSettings.format = dailyNotesSettings.format || "YYYY-MM-DD"
    delete dailyNotesSettings.template

    const elmDiv = document.createElement('div');
    elmDiv.id = "elm-node";
    this.containerEl.children[1].appendChild(elmDiv);

    const elm = Elm.Main.init({
      node: elmDiv,
      flags: dailyNotesSettings
    })

    const that = this;

    elm.ports.updateTodos.subscribe(function(data: { filePath: string, todos: [ { lineNumber: number, originalText: string, newText: string } ] }) {
      that.handleUpdateTodos(data);
    })

    elm.ports.deleteTodo.subscribe(function(data: {filePath: string, lineNumber: number, originalText: string }) {
      that.handleDeleteTodo(data);
    })

    elm.ports.editTodo.subscribe(function(data: {filePath: string, lineNumber: number, originalText: string }) {
      that.handleEditTodo(data);
    })

    elm.ports.displayTodoMarkdown.subscribe(function(data: { filePath: string, todoMarkdown: [{id: string, markdown: string }]}) {
      that.handleDisplayTodoMarkdown(data);
    })

    const markdownFiles = this.vault.getMarkdownFiles();

    for (const file of markdownFiles) {
      const fileDate = this.getFileDate(dailyNotesSettings.folder, dailyNotesSettings.format, file);

      const fileContents = await this.vault.cachedRead(file);
      elm.ports.fileAdded.send({
        filePath: file.path,
        fileDate: fileDate,
        fileContents: fileContents
      });
    }

    this.registerEvent(this.app.vault.on("create",
      (file) => this.handleFileCreated(elm, dailyNotesSettings, file)));

    this.registerEvent(this.app.vault.on("delete",
      (file) => this.handleFileDeleted(elm, dailyNotesSettings, file)));

    this.registerEvent(this.app.vault.on("modify",
      (file) => this.handleFileModified(elm, dailyNotesSettings, file)));
  }

  async handleDeleteTodo(data: {filePath: string, lineNumber: number, originalText: string}) {
    const file = this.app.vault.getAbstractFileByPath(data.filePath)
    if (file instanceof TFile) {
      const markdown = await this.vault.read(file)
      const markdownLines = markdown.split(/\r?\n/)
      if (markdownLines[data.lineNumber - 1].includes(data.originalText)) {
        markdownLines[data.lineNumber - 1] = markdownLines[data.lineNumber - 1].replace(/^(.*)$/, "<del>$1</del>")
        this.vault.modify(file, markdownLines.join("\n"))
      }
    }
  }

  async handleEditTodo(data: {filePath: string, lineNumber: number, originalText: string}) {
    const leaves = this.app.workspace.getLeavesOfType("markdown")

    let fileLeaf = null;

    for (const leaf of leaves) {
      const view = leaf.view
      if (view instanceof FileView) {
        if (data.filePath == view.file.path) {
          fileLeaf = leaf
        }

      }
    }

    if (fileLeaf == null) {
      const leaf = this.app.workspace.splitActiveLeaf();
      const file = this.app.vault.getAbstractFileByPath(data.filePath)
      if (file instanceof TFile) {
        await leaf.openFile(file);
      }
      fileLeaf = leaf
    }

    if (fileLeaf) {
      this.app.workspace.setActiveLeaf(fileLeaf, true, true)
      this.codeMirror.setCursor(data.lineNumber - 1, 0)
    }
  }

  async handleUpdateTodos(data: { filePath: string, todos: [ { lineNumber: number, originalText: string, newText: string } ] }) {
      const file = this.app.vault.getAbstractFileByPath(data.filePath)
      if (file instanceof TFile) {
        const markdown = await this.vault.read(file)
        const markdownLines = markdown.split(/\r?\n/)
        for (const item of data.todos) {
          if (markdownLines[item.lineNumber - 1].includes(item.originalText)) {
            markdownLines[item.lineNumber - 1] = item.newText
          }
        }
        this.vault.modify(file, markdownLines.join("\n"))
     }
  }

  async handleDisplayTodoMarkdown(data: { filePath: string, todoMarkdown: [{id: string, markdown: string }]}) {
    requestAnimationFrame(function () {
      for (const item of data.todoMarkdown) {
        const element = document.getElementById(item.id);
        if (element instanceof HTMLElement) {
          element.innerHTML = "";
          MarkdownRenderer.renderMarkdown(item.markdown, element, data.filePath, this);
        }
      }
    })
  }

  async handleFileCreated(elm: any, dailyNotesSettings: any, file: TAbstractFile) {

    if (file instanceof TFile) {
      const fileDate = this.getFileDate(dailyNotesSettings.folder, dailyNotesSettings.format, file);
      const fileContents = await this.vault.read(file);

      elm.ports.fileAdded.send({
        filePath: file.path,
        fileDate: fileDate,
        fileContents: fileContents
      })
    }
  }

  async handleFileDeleted(elm: any, dailyNotesSettings: any, file: TAbstractFile) {

    if (file instanceof TFile) {
      const fileDate = this.getFileDate(dailyNotesSettings.folder, dailyNotesSettings.format, file);

      elm.ports.fileDeleted.send(file.path)
    }
  }

  async handleFileModified(elm: any, dailyNotesSettings: any, file: TAbstractFile) {

    if (file instanceof TFile) {
      const fileDate = this.getFileDate(dailyNotesSettings.folder, dailyNotesSettings.format, file);
      const fileContents = await this.vault.read(file);

      elm.ports.fileUpdated.send({
        filePath: file.path,
        fileDate: fileDate,
        fileContents: fileContents
      })
    }
  }

  getDisplayText(): string {
    return 'Card Board';
  }

  getViewType(): string {
    return 'CardBoard';
  }

  getFileDate(dailyNotesFolder: string, dailyNotesNameFormat: string, file: any): string | null {
    var fileDate = null;
    if (dailyNotesFolder != null) {
      if (file.path.startsWith(dailyNotesFolder)) {
        fileDate = moment(file.basename, dailyNotesNameFormat).format('YYYY-MM-DD');
      }
    }
    return fileDate
  }
}
