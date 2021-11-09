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

const moment = require('moment');

import { Elm, ElmApp, Flags } from '../src/Main';

import CardBoardPlugin from './main';

import { getDateFromFile, IPeriodicNoteSettings } from 'obsidian-daily-notes-interface';

export const VIEW_TYPE_CARD_BOARD = "card-board-view";

export class CardBoardView extends ItemView {
  private vault:  Vault;
  private plugin: CardBoardPlugin;
  private elm: ElmApp;

  constructor(
    plugin: CardBoardPlugin,
    leaf: WorkspaceLeaf
  ) {
    super(leaf);
    this.plugin = plugin;
    this.app = plugin.app;
    this.vault = plugin.app.vault;
  }

  getViewType(): string {
    return VIEW_TYPE_CARD_BOARD;
  }

  getDisplayText(): string {
    return 'CardBoard';
  }

  async onOpen() {
    const mySettings:Flags = {
      now:          Date.now(),
      zone:         new Date().getTimezoneOffset(),
      settings:     this.plugin.settings
    };

    const elmDiv = document.createElement('div');
    elmDiv.id = "elm-node";
    this.containerEl.children[1].appendChild(elmDiv);

    this.elm = Elm.Main.init({
      node: elmDiv,
      flags: mySettings
    })

    // TODO: I know I shouldn't need to do this, but my js foo
    // failed me at the time!
    const that = this;

    // messages from elm code.  This is the only route
    // that elm has to the obsidian API, so this is the
    // entry point for anything side-effecty
    this.elm.ports.interopFromElm.subscribe((fromElm) => {
      switch (fromElm.tag) {
        case "addFilePreviewHovers":
          that.handleAddFilePreviewHovers(fromElm.data);
          break;
        case "closeView":
          that.handleCloseView();
          break;
        case "deleteTask":
          that.handleDeleteTask(fromElm.data);
          break;
        case "displayTaskMarkdown":
          that.handleDisplayTaskMarkdown(fromElm.data);
          break;
        case "openTaskSourceFile":
          that.handleOpenTaskSourceFile(fromElm.data);
          break;
        case "updateSettings":
          that.handleUpdateSettings(fromElm.data);
          break;
        case "updateTasks":
          that.handleUpdateTasks(fromElm.data);
          break;
      }
    });

    const markdownFiles = this.vault.getMarkdownFiles();

    for (const file of markdownFiles) {
      const fileDate      = this.formattedFileDate(file);
      const fileContents  = await this.vault.cachedRead(file);
      this.elm.ports.interopToElm.send({
        tag: "fileAdded",
        data: {
          filePath:     file.path,
          fileDate:     fileDate,
          fileContents: fileContents
        }
      });
    }

    this.elm.ports.interopToElm.send({
      tag: "initCompleted",
      data: { }
    });

    this.registerEvent(this.app.workspace.on("active-leaf-change",
      (leaf) => this.handleActiveLeafChange(leaf)));

    this.registerEvent(this.app.vault.on("create",
      (file) => this.handleFileCreated(file)));

    this.registerEvent(this.app.vault.on("delete",
      (file) => this.handleFileDeleted(file)));

    this.registerEvent(this.app.vault.on("modify",
      (file) => this.handleFileModified(file)));

    this.registerEvent(this.app.vault.on("rename",
      (file, oldPath) => this.handleFileRenamed(file, oldPath)));
  }

  async onClose() {
    this.elm.ports.interopToElm.send({
      tag: "activeStateUpdated",
      data: false
    });
  }

  currentBoardIndex(index: number) {
    this.elm.ports.interopToElm.send({
      tag: "showBoard",
      data: index
    });
  }

  // MESSAGES FROM ELM

  async handleAddFilePreviewHovers(
    data: {
      filePath: string,
      id : string
    }[]
  ) {
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

  async handleCloseView() {
    this.plugin.deactivateView();
  }

  async handleDeleteTask(
    data: {
      filePath: string,
      lineNumber: number,
      originalText: string}
  ) {
    const file = this.app.vault.getAbstractFileByPath(data.filePath)

    if (file instanceof TFile) {
      const markdown      = await this.vault.read(file)
      const markdownLines = markdown.split(/\r?\n/)

      if (markdownLines[data.lineNumber - 1].includes(data.originalText)) {
        markdownLines[data.lineNumber - 1] = markdownLines[data.lineNumber - 1].replace(/^(.*)$/, "<del>$1</del>")
        this.vault.modify(file, markdownLines.join("\n"))
      }
    }
  }

  async handleDisplayTaskMarkdown(
    data: {
      filePath: string,
      taskMarkdown: {
        id: string,
        markdown: string
      }[]
    }[]
  ) {
    const that = this;

    requestAnimationFrame(function () {
      for (const card of data) {
        for (const item of card.taskMarkdown) {
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

  async handleOpenTaskSourceFile(
    data: {
      filePath: string,
      lineNumber: number,
      originalText: string
    }
  ) {
    await this.openOrSwitchWithHighlight(this.app, data.filePath, data.lineNumber);
  }

  async handleUpdateSettings(
    data: {
      data : {
        boardConfigs : (
          { data : {
              columns : { displayTitle : string; tag : string }[];
              completedCount : number;
              includeOthers : boolean;
              includeUntagged : boolean;
              title : string
            };
            tag : "tagBoardConfig" }
          | { data : {
              completedCount : number;
              includeUndated : boolean;
              title : string
            };
            tag : "dateBoardConfig"
          }
        )[]
      };
      version : string
  }) {
    await this.plugin.saveSettings(data);

    this.elm.ports.interopToElm.send({
      tag: "settingsUpdated",
      data: data
    });
  }

  async handleUpdateTasks(
    data: {
      filePath: string,
      tasks: { lineNumber: number, originalText: string, newText: string }[]
  }) {
    const file = this.app.vault.getAbstractFileByPath(data.filePath)

    if (file instanceof TFile) {
      const markdown      = await this.vault.read(file)
      const markdownLines = markdown.split(/\r?\n/)

      for (const item of data.tasks) {
        if (markdownLines[item.lineNumber - 1].includes(item.originalText)) {
          markdownLines[item.lineNumber - 1] = item.newText
        }
      }
      this.vault.modify(file, markdownLines.join("\n"))
    }
  }

  // THESE SEND MESSAGES TO THE ELM APPLICATION
  async handleActiveLeafChange(
    leaf: WorkspaceLeaf | null
  ) {
    let isActive: boolean = false;

    if (leaf.view.getViewType() == "card-board-view") {
      isActive = true
    }

    this.elm.ports.interopToElm.send({
      tag: "activeStateUpdated",
      data: isActive
    });
  }


  async handleFileCreated(
    file: TAbstractFile
  ) {
    if (file instanceof TFile) {
      const fileDate      = this.formattedFileDate(file);
      const fileContents  = await this.vault.read(file);

      this.elm.ports.interopToElm.send({
        tag: "fileAdded",
        data: {
          filePath: file.path,
          fileDate: fileDate,
          fileContents: fileContents
        }
      });
    }
  }

  async handleFileDeleted(
    file: TAbstractFile
  ) {
    if (file instanceof TFile) {
      const fileDate = this.formattedFileDate(file);

      this.elm.ports.interopToElm.send({
        tag: "fileDeleted",
        data: file.path
      });
    }
  }

  async handleFileModified(
    file: TAbstractFile
  ) {
    if (file instanceof TFile) {
      const fileDate      = this.formattedFileDate(file);
      const fileContents  = await this.vault.read(file);

      this.elm.ports.interopToElm.send({
        tag: "fileUpdated",
        data: {
          filePath: file.path,
          fileDate: fileDate,
          fileContents: fileContents
        }
      });
    }
  }

  async handleFileRenamed(
    file: TAbstractFile,
    oldPath: string
  ) {
    this.elm.ports.interopToElm.send({
      tag: "fileRenamed",
      data: {
        oldPath: oldPath,
        newPath: file.path
      }
    });
  }

  // HELPERS

  formattedFileDate(
    file: TFile
  ): string | null {
    return getDateFromFile(file, "day")?.format('YYYY-MM-DD') || null;
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
