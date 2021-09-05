import { App, Modal, Notice, Plugin, PluginSettingTab, Setting, addIcon } from 'obsidian';
import { CardBoardView } from './view';

interface MyPluginSettings {
	mySetting: string;
}

const DEFAULT_SETTINGS: MyPluginSettings = {
	mySetting: 'default'
}

export default class CardBoardPlugin extends Plugin {
  settings: MyPluginSettings;
  codeMirror: CodeMirror.Editor;

  async onload() {
    console.log('loading CardBoard plugin');

    await this.loadSettings();

    this.registerCodeMirror((cm: CodeMirror.Editor) => {
      this.codeMirror = cm;
    });

    addIcon("card-board",
      '<rect x="2" y="2" width="96" height="96" rx="12" ry="12" fill="none" stroke="currentColor" stroke-width="5"></rect>' +
      '<rect x="28" y="28" width="12" height="46" fill="none" stroke="currentColor" stroke-width="5"></rect>' +
      '<rect x="56" y="28" width="12" height="20" fill="none" stroke="currentColor" stroke-width="5"></rect>');

    this.addRibbonIcon('card-board', 'CardBoard Plugin', async () => {
      const leaf = this.app.workspace.getLeaf(!(this.app.workspace.activeLeaf.view.getViewType() == 'empty'));
      const view = new CardBoardView(this.codeMirror, this.app, leaf);
      await leaf.open(view);
      this.app.workspace.setActiveLeaf(leaf, true, true);
    });
  }

  onunload() {
    console.log('unloading CardBoard plugin');
  }

  async loadSettings() {
    await this.loadData();
  }
}
