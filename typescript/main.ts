import { App, Modal, Notice, Plugin, PluginSettingTab, Setting, addIcon } from 'obsidian';
import { CardBoardView } from './view';

const DEFAULT_SETTINGS: ({ completedCount : number; includeUndated : boolean; tag : "dateBoardConfig"; title : string } | { columns : { displayTitle : string; tag : string }[]; completedCount : number; includeOthers : boolean; includeUntagged : boolean; tag : "tagBoardConfig"; title : string })[] = [];

export default class CardBoardPlugin extends Plugin {
  settings: ({ data : { completedCount : number; includeUndated : boolean; title : string }; tag : "dateBoardConfig" } | { data : { columns : { displayTitle : string; tag : string }[]; completedCount : number; includeOthers : boolean; includeUntagged : boolean; title : string }; tag : "tagBoardConfig" })[];

  async onload() {
    console.log('loading CardBoard plugin');

    await this.loadSettings();

    addIcon("card-board",
      '<rect x="2" y="2" width="96" height="96" rx="12" ry="12" fill="none" stroke="currentColor" stroke-width="5"></rect>' +
      '<rect x="28" y="28" width="12" height="46" fill="none" stroke="currentColor" stroke-width="5"></rect>' +
      '<rect x="56" y="28" width="12" height="30" fill="none" stroke="currentColor" stroke-width="5"></rect>');

    this.addRibbonIcon('card-board', 'CardBoard Plugin', async () => {
      const leaf = this.app.workspace.getLeaf(!(this.app.workspace.activeLeaf.view.getViewType() == 'empty'));
      const view = new CardBoardView(this, leaf);
      await leaf.open(view);
      this.app.workspace.setActiveLeaf(leaf, true, true);
    });
  }

  onunload() {
    console.log('unloading CardBoard plugin');
  }

  async loadSettings() {
    // this.settings = Object.assign([], DEFAULT_SETTINGS, await this.loadData());
    this.settings = await this.loadData();
  }

  async saveSettings(newSettings: ({ data : { completedCount : number; includeUndated : boolean; title : string }; tag : "dateBoardConfig" } | { data : { columns : { displayTitle : string; tag : string }[]; completedCount : number; includeOthers : boolean; includeUntagged : boolean; title : string }; tag : "tagBoardConfig" })[]) {
    await this.saveData(newSettings);
  }
}
