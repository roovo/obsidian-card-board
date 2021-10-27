import {
  App,
  FileView,
  ItemView,
  MarkdownRenderer,
  TAbstractFile,
  TFile,
  Vault,
  WorkspaceLeaf
} from 'obsidian';

import { Elm } from '../src/Main';

import CardBoardPlugin from './main';

const moment = require('moment');

export class CardBoardView extends ItemView {
  private vault: Vault;
  private plugin: CardBoardPlugin;

  constructor(plugin: CardBoardPlugin, leaf: WorkspaceLeaf) {
    super(leaf);
    this.plugin = plugin;
    this.app = plugin.app;
    this.vault = plugin.app.vault;
  }

  async onOpen() {
    // @ts-ignore
    const dailyNotesSettings  = this.app.internalPlugins.getPluginById("daily-notes")?.instance?.options;
    dailyNotesSettings.folder = dailyNotesSettings.folder || ""
    dailyNotesSettings.format = dailyNotesSettings.format || "YYYY-MM-DD"
    dailyNotesSettings.now    = Date.now()
    dailyNotesSettings.zone   = new Date().getTimezoneOffset()

    const elmDiv = document.createElement('div');
    elmDiv.id = "elm-node";
    this.containerEl.children[1].appendChild(elmDiv);

    const elm = Elm.Main.init({
      node: elmDiv,
      flags: dailyNotesSettings
    })

    const that = this;

    elm.ports.interopFromElm.subscribe((fromElm) => {
      switch (fromElm.tag) {
        case "addFilePreviewHovers":
          that.handleAddFilePreviewHovers(fromElm.data);
          break;
        case "deleteTodo":
          that.handleDeleteTodo(fromElm.data);
          break;
        case "displayTodoMarkdown":
          that.handleDisplayTodoMarkdown(fromElm.data);
          break;
        case "openTodoSourceFile":
          that.handleOpenTodoSourceFile(fromElm.data);
          break;
        case "updateConfig":
          that.handleUpdateConfig(fromElm.data);
          break;
        case "updateTodos":
          that.handleUpdateTodos(fromElm.data);
          break;
      }
    });

    const markdownFiles = this.vault.getMarkdownFiles();

    for (const file of markdownFiles) {
      const fileDate = this.getFileDate(dailyNotesSettings.folder, dailyNotesSettings.format, file);

      const fileContents = await this.vault.cachedRead(file);
      elm.ports.interopToElm.send({
        tag: "fileAdded",
        data: { filePath: file.path,
          fileDate: fileDate,
          fileContents: fileContents
        }
      });
    }

    elm.ports.interopToElm.send({
      tag: "initCompleted",
      data: { }
    });

    this.registerEvent(this.app.vault.on("create",
      (file) => this.handleFileCreated(elm, dailyNotesSettings, file)));

    this.registerEvent(this.app.vault.on("delete",
      (file) => this.handleFileDeleted(elm, dailyNotesSettings, file)));

    this.registerEvent(this.app.vault.on("modify",
      (file) => this.handleFileModified(elm, dailyNotesSettings, file)));
  }

  async handleAddFilePreviewHovers(data: {filePath: string, id : string }[]) {
    const that = this;
    requestAnimationFrame(function () {
      for (const card of data) {
        const element = document.getElementById(card.id);
        if (element instanceof HTMLElement) {
          element.addEventListener('mouseover', (event: MouseEvent) => {
            that.app.workspace.trigger('hover-link', {
              event,
              source: "card-board",
              hoverParent: element,
              targetEl: element,
              linktext: card.filePath,
              sourcePath: card.filePath
            });
          });
        }
      }
    })
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

  async handleDisplayTodoMarkdown(data: { filePath: string, todoMarkdown: {id: string, markdown: string }[]}[]) {
    const that = this;
    requestAnimationFrame(function () {
      for (const card of data) {
        for (const item of card.todoMarkdown) {
          const element = document.getElementById(item.id);
          if (element instanceof HTMLElement) {
            element.innerHTML = "";
            MarkdownRenderer.renderMarkdown(item.markdown, element, card.filePath, this);

            const internalLinks = Array.from(element.getElementsByClassName("internal-link"));

            for (const internalLink of internalLinks) {
              if (internalLink instanceof HTMLElement) {
                internalLink.addEventListener('mouseover', (event: MouseEvent) => {
                  that.app.workspace.trigger('hover-link', {
                    event,
                    source: "card-board",
                    hoverParent: element,
                    targetEl: internalLink,
                    linktext: internalLink.getAttribute("href"),
                    sourcePath: card.filePath
                  });
                });

                internalLink.addEventListener("click", (event: MouseEvent) => {
                    event.preventDefault();
                    that.app.workspace.openLinkText(internalLink.getAttribute("href"), card.filePath, true, {
                        active: !0
                    });
                });
              }
            }
          }
        }
      }
    })
  }

  async handleOpenTodoSourceFile(data: { filePath: string, blockLink: (string | null), lineNumber: number, originalText: string }) {
    var linkText = ""
    if (data.blockLink == null) {
      const blockLink = "^" + this.generateId();
      const newText   = data.originalText + " " + blockLink;

      await this.handleUpdateTodos({
        filePath: data.filePath,
        todos: [ {
          lineNumber: data.lineNumber,
          originalText: data.originalText,
          newText: newText
        } ]
      });

      // TODO: this is an icky hack to ensure that the cache
      // has caught up before I call openLinkText
      await this.delay(300);

      linkText = data.filePath + "#" + blockLink
    } else {
      linkText = data.filePath + "#" + data.blockLink
    }
    this.app.workspace.openLinkText(linkText, data.filePath, true, { active: !0 });
  }

  delay(ms: number) {
    return new Promise( resolve => setTimeout(resolve, ms) );
  }

  // async handleUpdateConfig(data: { filePath: string, todos: { lineNumber: number, originalText: string, newText: string }[] }) {
  async handleUpdateConfig(data: any ) {
    this.plugin.saveSettings(data);
  }

  async handleUpdateTodos(data: { filePath: string, todos: { lineNumber: number, originalText: string, newText: string }[] }) {
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

  async handleFileCreated(elm: any, dailyNotesSettings: any, file: TAbstractFile) {

    if (file instanceof TFile) {
      const fileDate = this.getFileDate(dailyNotesSettings.folder, dailyNotesSettings.format, file);
      const fileContents = await this.vault.read(file);

      elm.ports.interopToElm.send({
        tag: "fileAdded",
        data: { filePath: file.path,
          fileDate: fileDate,
          fileContents: fileContents
        }
      });
    }
  }

  async handleFileDeleted(elm: any, dailyNotesSettings: any, file: TAbstractFile) {

    if (file instanceof TFile) {
      const fileDate = this.getFileDate(dailyNotesSettings.folder, dailyNotesSettings.format, file);

      elm.ports.interopToElm.send({
        tag: "fileDeleted",
        data: file.path
        });
    }
  }

  async handleFileModified(elm: any, dailyNotesSettings: any, file: TAbstractFile) {

    if (file instanceof TFile) {
      const fileDate = this.getFileDate(dailyNotesSettings.folder, dailyNotesSettings.format, file);
      const fileContents = await this.vault.read(file);

      elm.ports.interopToElm.send({
        tag: "fileUpdated",
        data: { filePath: file.path,
          fileDate: fileDate,
          fileContents: fileContents
        }
      });
    }
  }

  getDisplayText(): string {
    return 'Card Board';
  }

  getViewType(): string {
    return 'CardBoard';
  }

  generateId(): string {
    return Math.random().toString(36).substr(2, 6);
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
