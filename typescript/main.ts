import { App, Modal, Notice, Plugin, PluginSettingTab, Setting } from 'obsidian';
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

    this.addRibbonIcon('dice', 'CardBoard Plugin', async () => {
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
