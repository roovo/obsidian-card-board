import { App, Modal, Notice, Plugin, PluginSettingTab, Setting, Vault, addIcon } from 'obsidian';
import { CardBoardView } from './view';

import { Elm } from '../src/Main';

const moment = require('moment');

interface MyPluginSettings {
	mySetting: string;
}

const DEFAULT_SETTINGS: MyPluginSettings = {
	mySetting: 'default'
}

export default class CardBoardPlugin extends Plugin {
  settings: MyPluginSettings;
  codeMirror: CodeMirror.Editor;
  elm: any;

  async onload() {
    console.log('loading CardBoard plugin');

    await this.loadSettings();

    this.registerCodeMirror((cm: CodeMirror.Editor) => {
      this.codeMirror = cm;
    });

    addIcon("card-board",
      '<rect x="2" y="2" width="96" height="96" rx="12" ry="12" fill="none" stroke="currentColor" stroke-width="5"></rect>' +
      '<rect x="28" y="28" width="12" height="46" fill="none" stroke="currentColor" stroke-width="5"></rect>' +
      '<rect x="56" y="28" width="12" height="30" fill="none" stroke="currentColor" stroke-width="5"></rect>');

    this.addRibbonIcon('card-board', 'CardBoard Plugin', async () => {
      const leaf = this.app.workspace.getLeaf(!(this.app.workspace.activeLeaf.view.getViewType() == 'empty'));
      const view = new CardBoardView(this.codeMirror, this.app, leaf);
      await leaf.open(view);
      this.app.workspace.setActiveLeaf(leaf, true, true);
    });

    // @ts-ignore
    const dailyNotesSettings  = this.app.internalPlugins.getPluginById("daily-notes")?.instance?.options;
    dailyNotesSettings.folder = dailyNotesSettings.folder || ""
    dailyNotesSettings.format = dailyNotesSettings.format || "YYYY-MM-DD"
    dailyNotesSettings.now    = Date.now()
    dailyNotesSettings.zone   = new Date().getTimezoneOffset()

    // @ts-ignore
    this.elm = Elm.Worker.init({
      flags: dailyNotesSettings
    });

    // @ts-ignore
    this.registerEvent(this.app.workspace.onLayoutReady(async () => await this.loadTaskItems()));
  }

  onunload() {
    console.log('unloading CardBoard plugin');
  }

  async loadTaskItems(): Promise<void> {
    console.log('CardBoard plugin layout ready');

    // @ts-ignore
    const dailyNotesSettings  = this.app.internalPlugins.getPluginById("daily-notes")?.instance?.options;
    dailyNotesSettings.folder = dailyNotesSettings.folder || ""
    dailyNotesSettings.format = dailyNotesSettings.format || "YYYY-MM-DD"
    dailyNotesSettings.now    = Date.now()
    dailyNotesSettings.zone   = new Date().getTimezoneOffset()

    const markdownFiles = this.app.vault.getMarkdownFiles();

    for (const file of markdownFiles) {
      const fileDate = this.getFileDate(dailyNotesSettings.folder, dailyNotesSettings.format, file);

      const fileContents = await this.app.vault.cachedRead(file);
      console.log(file.path);
      this.elm.ports.interopToElm.send({
        tag: "fileAdded",
        data: { filePath: file.path,
          fileDate: fileDate,
          fileContents: fileContents
        }
      });
    }
  }

  async loadSettings() {
    await this.loadData();
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
