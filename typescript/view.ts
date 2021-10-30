import {
  App,
  FileView,
  ItemView,
  MarkdownRenderer,
  MarkdownView,
  TAbstractFile,
  TFile,
  Vault,
  WorkspaceLeaf
} from 'obsidian';

import { Elm, ElmApp, Flags } from '../src/Main';

import CardBoardPlugin from './main';

const moment = require('moment');

export const VIEW_TYPE_CARD_BOARD = "card-board-view";

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
    const dailyNotesSettings = this.app.internalPlugins.getPluginById("daily-notes")?.instance?.options;

    const mySettings:Flags = {
      now:          Date.now(),
      zone:         new Date().getTimezoneOffset(),
      boardConfigs: this.plugin.settings?.data?.boardConfigs
    };

    const elmDiv = document.createElement('div');
    elmDiv.id = "elm-node";
    this.containerEl.children[1].appendChild(elmDiv);

    const elm:ElmApp = Elm.Main.init({
      node: elmDiv,
      flags: mySettings
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
        case "updateSettings":
          that.handleUpdateSettings(elm, fromElm.data);
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

    this.registerEvent(this.app.vault.on("rename",
      (file, oldPath) => this.handleFileRenamed(elm, dailyNotesSettings, file, oldPath)));
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

  async handleOpenTodoSourceFile(
    data: { filePath: string,
            blockLink: (string | null),
            lineNumber: number,
            originalText: string }) {

    await this.openOrSwitchWithHighlight(this.app, data.filePath, data.lineNumber);
  }

  async handleUpdateSettings(elm: ElmApp, data: { data : { boardConfigs : ({ data : { columns : { displayTitle : string; tag : string }[]; completedCount : number; includeOthers : boolean; includeUntagged : boolean; title : string }; tag : "tagBoardConfig" } | { data : { completedCount : number; includeUndated : boolean; title : string }; tag : "dateBoardConfig" })[] }; version : string }) {
    await this.plugin.saveSettings(data);
    elm.ports.interopToElm.send({
      tag: "settingsUpdated",
      data: data
    });
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

  async handleFileCreated(elm: ElmApp, dailyNotesSettings: any, file: TAbstractFile) {

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

  async handleFileRenamed(elm: any, dailyNotesSettings: any, file: TAbstractFile, oldPath:string) {
    elm.ports.interopToElm.send({
      tag: "fileRenamed",
      data: { oldPath: oldPath,
        newPath: file.path
      }
    });
  }

  getDisplayText(): string {
    return 'CardBoard';
  }

  getViewType(): string {
    return VIEW_TYPE_CARD_BOARD;
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

  async openOrSwitchWithHighlight(
    app: App,
    filePath: string,
    lineNumber: number
  ): Promise<void> {
    const { workspace } = app;

    let destFile = app.metadataCache.getFirstLinkpathDest(filePath, "");

    if (!destFile) {
       return;
    }

    const leavesWithDestAlreadyOpen: WorkspaceLeaf[] = [];

    workspace.iterateAllLeaves((leaf) => {
      if (leaf.view instanceof MarkdownView) {
        if (leaf.view?.file?.path === filePath) {
          leavesWithDestAlreadyOpen.push(leaf);
        }
      }
    });

    let leaf: WorkspaceLeaf;

    if (leavesWithDestAlreadyOpen.length > 0) {
      leaf = leavesWithDestAlreadyOpen[0];
      await workspace.setActiveLeaf(leaf);
    } else {
      leaf = workspace.splitActiveLeaf();
      await leaf.openFile(destFile);
    }

    leaf.setEphemeralState({ line: lineNumber - 1 });
  }
}
