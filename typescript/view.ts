import {ItemView, App, Vault, WorkspaceLeaf} from 'obsidian';
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

    const app = Elm.Main.init({
      node: elmDiv,
      flags: dailyNotesSettings
    })

    const markdownFiles = this.vault.getMarkdownFiles();

    for (const file of markdownFiles) {
      var fileDate = null;
      if (dailyNotesSettings.folder != null) {
        if (file.path.startsWith(dailyNotesSettings.folder)) {
          fileDate = moment(file.basename, dailyNotesSettings.format).format('YYYY-MM-DD');
        }
      }
      const fileContents = await this.vault.cachedRead(file);
      app.ports.dataForElm.send({
        tag: "MarkdownToParse",
        data: {
          fileDate: fileDate,
          fileContents: fileContents
        }
      });
    }
  }

  getDisplayText(): string {
    return 'Kanban';
  }

  getViewType(): string {
    return 'Kanban';
  }
}
