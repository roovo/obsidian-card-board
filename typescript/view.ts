import { App, ItemView, TAbstractFile, TFile, Vault, WorkspaceLeaf} from 'obsidian';
import { Elm } from '../src/Main';
import KanbanPlugin from './main';

const moment = require('moment');

export class KanbanView extends ItemView {
  private vault: Vault;

  constructor(app: App, leaf: WorkspaceLeaf) {
    super(leaf);
    this.app = app;
    this.vault = app.vault;
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

    const markdownFiles = this.vault.getMarkdownFiles();

    for (const file of markdownFiles) {
      const fileDate = this.getFileDate(dailyNotesSettings.folder, dailyNotesSettings.format, file);

      const fileContents = await this.vault.cachedRead(file);
      elm.ports.dataForElm.send({
        tag: "MarkdownToParse",
        data: {
          filePath: file.path,
          fileDate: fileDate,
          fileContents: fileContents
        }
      });
    }

    this.registerEvent(this.app.vault.on("modify",
      (file) => this.handleFileModified(elm, dailyNotesSettings, file)));
  }

  async handleFileModified(elm: any, dailyNotesSettings: any, file: TAbstractFile) {

    if (file instanceof TFile) {
      const fileDate = this.getFileDate(dailyNotesSettings.folder, dailyNotesSettings.format, file);
      const fileContents = await this.vault.read(file);

      elm.ports.dataForElm.send({
        tag: "FileModified",
        data: {
          filePath: file.path,
          fileDate: fileDate,
          fileContents: fileContents
        }
      })
    }
  }

  getDisplayText(): string {
    return 'Kanban';
  }

  getViewType(): string {
    return 'Kanban';
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
