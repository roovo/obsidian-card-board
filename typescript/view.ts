import {
  App,
  FileView,
  ItemView,
  MarkdownRenderer,
  MarkdownView,
  TAbstractFile,
  TFile,
  TFolder,
  Vault,
  WorkspaceLeaf
} from 'obsidian';

import { Elm, ElmApp, Flags } from '../src/Main';

import CardBoardPlugin from './main';
import { CardBoardPluginSettingsPostV11 } from './types';
import { getDateFromFile, IPeriodicNoteSettings } from 'obsidian-daily-notes-interface';
import { FileFilter } from './fileFilter'

export const VIEW_TYPE_CARD_BOARD = "card-board-view";

export class CardBoardView extends ItemView {
  private vault:      Vault;
  private plugin:     CardBoardPlugin;
  private elm:        ElmApp;
  private elmDiv:     any;
  private fileFilter: FileFilter;

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
    this.icon       = "card-board"

    const globalSettings : any = this.plugin.settings?.data.globalSettings;

    if ((!(globalSettings === undefined)) && globalSettings.hasOwnProperty('filters')) {
      this.fileFilter = new FileFilter(globalSettings.filters);
    } else {
      this.fileFilter = new FileFilter([]);
    }

    // @ts-ignore
    const dataviewSettings = this.app.plugins.getPlugin("dataview")?.settings

    const mySettings:Flags = {
      uniqueId:           this.randomId(5),
      now:                Date.now(),
      zone:               new Date().getTimezoneOffset(),
      settings:           this.plugin.settings,
      rightToLeft:        (this.app.vault as any).getConfig("rightToLeft"),
      dataviewTaskCompletion:   {
        taskCompletionTracking:           dataviewSettings  === undefined ? true          : dataviewSettings['taskCompletionTracking'],
        taskCompletionUseEmojiShorthand:  dataviewSettings  === undefined ? false         : dataviewSettings['taskCompletionUseEmojiShorthand'],
        taskCompletionText:               dataviewSettings  === undefined ? "completion"  : dataviewSettings['taskCompletionText']
      }
    };

    this.elmDiv = document.createElement('div');
    this.elmDiv.id = "elm-node";
    this.containerEl.children[1].appendChild(this.elmDiv);

    this.elm = Elm.Main.init({
      node: this.elmDiv,
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
        case "elmInitialized":
          that.handleElmInitialized();
          break;
        case "openTaskSourceFile":
          that.handleOpenTaskSourceFile(fromElm.data);
          break;
        case "requestFilterCandidates":
          that.handleRequestFilterCandidates();
          break;
        case "trackDraggable":
          that.handleTrackDraggable(fromElm.data);
          break;
        case "updateSettings":
          that.handleUpdateSettings(fromElm.data);
          break;
        case "updateTasks":
          that.handleUpdateTasks(fromElm.data);
          break;
      }
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

    // @ts-ignore
    this.registerEvent(this.app.vault.on("config-changed",
      () => this.handleConfigChanged()));
  }

  async onClose() {
    await this.elm.ports.interopToElm.send({
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
            MarkdownRenderer.render(that.app, item.markdown, element, card.filePath, that);

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

            const tags = Array.from(element.getElementsByClassName("tag"));

            for (const tag of tags) {
              if (tag instanceof HTMLElement) {
                tag.addEventListener("mouseup", (event: MouseEvent) => {
                  event.preventDefault();

                  (that.app as any).internalPlugins.plugins["global-search"].instance.openGlobalSearch("tag:"+ (event.target as HTMLElement).textContent);
                });
              }
            }
          }
        }
      }
    })
  }

  async handleElmInitialized() {
    const markdownFiles = this.vault.getMarkdownFiles();
    const filteredFiles = markdownFiles.filter((file) => this.fileFilter.isAllowed(file.path));

    for (const file of filteredFiles) {
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
      tag: "allMarkdownLoaded",
      data: { }
    });
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

  async handleRequestFilterCandidates() {
    const loadedFiles = this.app.vault.getAllLoadedFiles();
    const filterCandidates: { tag : "pathFilter" | "fileFilter" | "tagFilter", data : string }[] = [];
    // @ts-ignore
    const tagsWithCounts = this.app.metadataCache.getTags();
    const tags = Object.keys(tagsWithCounts).map(x => x.slice(1));

    loadedFiles.forEach((folder: TAbstractFile) => {
      if (folder instanceof TFolder) {
        filterCandidates.push({ tag : "pathFilter", data : folder.path});
      }
    });

    loadedFiles.forEach((file: TAbstractFile) => {
      if (file instanceof TFile && file.extension === "md") {
        filterCandidates.push({ tag : "fileFilter", data : file.path});
      }
    });

    tags.forEach((tag: string) => {
      filterCandidates.push({ tag : "tagFilter", data : tag});
      filterCandidates.push({ tag : "tagFilter", data : tag + "/"});
    });

    this.elm.ports.interopToElm.send({
      tag: "filterCandidates",
      data: filterCandidates
    });
  }

  async handleTrackDraggable(
    data : {
      dragType: string,
      clientPos : { x: number, y: number },
      draggableId: string
    }
  ) {
    const MINIMUM_DRAG_PIXELS = 10;

    const that = this;

    const draggedElement = document.getElementById(data.draggableId);
    const beaconType = "data-" + data.dragType + "-beacon";
    const dragContainer = data.dragType + "-container";

    document.addEventListener("mousemove", maybeDragMove);
    document.addEventListener("mouseup", stopAwaitingDrag);

    console.log("tracking: " + beaconType);

    function maybeDragMove(moveEvent: MouseEvent) {
      const dragDistance = distance({ x: data.clientPos.x, y: data.clientPos.y}, coords(moveEvent));

      if (dragDistance >= MINIMUM_DRAG_PIXELS) {
        dragEvent("move", moveEvent);

        document.removeEventListener("mousemove", maybeDragMove);
        document.removeEventListener("mouseup", stopAwaitingDrag);

        document.addEventListener("mousemove", dragMove);
        document.addEventListener("mouseup", dragEnd);
      }
    }

    function dragMove(event: MouseEvent) {
      dragEvent("move", event);
    }

    function dragEnd(event: MouseEvent) {
      dragEvent("stop", event);
      document.removeEventListener("mousemove", dragMove);
      document.removeEventListener("mouseup", dragEnd);
    }

    function stopAwaitingDrag(event: MouseEvent) {
      dragEvent("stop", event);
      document.removeEventListener("mousemove", maybeDragMove);
      document.removeEventListener("mouseup", stopAwaitingDrag);
    }

    function dragEvent(dragAction: "move" | "stop", event: MouseEvent) {
      const tabHeader   = document.getElementsByClassName("workspace-tab-header-container")[1];
      const ribbon      = document.getElementsByClassName("workspace-ribbon")[0];
      const leftSplit   = document.getElementsByClassName("workspace-split")[0];

      const scrollable    = document.getElementsByClassName(dragContainer)[0];
      const SCROLL_MARGIN = 20;

      if (scrollable instanceof HTMLElement) {
        const scrollableRect  = scrollable.getBoundingClientRect();
        const bottom          = scrollableRect.y + scrollableRect.height;

        if ( event.clientY < scrollableRect.y + SCROLL_MARGIN) {
          scrollable.scrollTop -= 5;
        } else if ( event.clientY > bottom - SCROLL_MARGIN) {
          scrollable.scrollTop += 5;
        }
      }

      if ((ribbon instanceof HTMLElement) &&
          (leftSplit instanceof HTMLElement) &&
          (draggedElement instanceof HTMLElement)) {
          const offsetLeft = ribbon.clientWidth + leftSplit.clientWidth;
          const offsetTop  = tabHeader.clientHeight;

          const draggedElementRect = draggedElement.getBoundingClientRect();

          that.elm.ports.interopToElm.send({
            tag: "elementDragged",
            data: {
              dragType: data.dragType,
              dragAction: dragAction,
              cursor: coords(event),
              offset: { x: offsetLeft, y: offsetTop },
              draggedNodeRect: {
                x: draggedElementRect.x,
                y: draggedElementRect.y,
                width: draggedElementRect.width,
                height: draggedElementRect.height
              },
              beacons: beaconPositions(beaconType)
            }
          });
      }
    }

    function beaconPositions(beaconType: String) {
      const beaconElements = document.querySelectorAll(`[${beaconType}]`);
      return Array.from(beaconElements).map(beaconData);
    }

    function beaconData(elem: Element) {
      const boundingRect = elem.getBoundingClientRect();
      const beaconId = elem.getAttribute(beaconType);
      return {
        beaconPosition: tryParse(beaconId),
        rect: {
          x: boundingRect.x,
          y: boundingRect.y,
          width: boundingRect.width,
          height: boundingRect.height
        }
      };
    }

    function tryParse(str: string) {
      try {
        return JSON.parse(str);
      } catch (e) {
        return str;
      }
    }

    function coords(event: MouseEvent) {
      return { x: event.clientX, y: event.clientY };
    }

    function distance(pos1: { x : number, y : number }, pos2: { x : number, y : number }) {
      const dx = pos1.x - pos2.x;
      const dy = pos1.y - pos2.y;
      return Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2));
    }
  }

  async handleUpdateSettings(data : CardBoardPluginSettingsPostV11) {
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

  async handleConfigChanged() {
    this.elm.ports.interopToElm.send({
      tag: "configChanged",
      data: {
        rightToLeft: (this.app.vault as any).getConfig("rightToLeft"),
      }
    });
  }

  async handleFileCreated(
    file: TAbstractFile
  ) {
    if (file instanceof TFile) {
      if (this.fileFilter.isAllowed(file.path)) {
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
  }

  async handleFileDeleted(
    file: TAbstractFile
  ) {
    if (file instanceof TFile) {
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
      if (this.fileFilter.isAllowed(file.path)) {
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
  }

  async handleFileRenamed(
    file: TAbstractFile,
    oldPath: string
  ) {
    let oldNew : [boolean, boolean] = [this.fileFilter.isAllowed(oldPath), this.fileFilter.isAllowed(file.path)];

    switch(oldNew.join(",")) {
      case 'false,true': {
        this.handleFileCreated(file)
        break;
      }
      case 'true,false': {
        this.elm.ports.interopToElm.send({
          tag: "fileDeleted",
          data: oldPath
        });
        break;
      }
      case 'true,true': {
        this.elm.ports.interopToElm.send({
          tag: "fileRenamed",
          data: {
            oldPath: oldPath,
            newPath: file.path
          }
        });
        break;
      }
    }
  }

  // HELPERS

  formattedFileDate(
    file: TFile
  ): string | null {
    return getDateFromFile(file, "day")?.format('YYYY-MM-DD') || null;
  }

  randomId(length: number): string {
      let result = '';
      const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
      const charactersLength = characters.length;
      let counter = 0;
      while (counter < length) {
        result += characters.charAt(Math.floor(Math.random() * charactersLength));
        counter += 1;
      }
      return result;
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
