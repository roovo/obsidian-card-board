import { App, Modal, Notice, Plugin, PluginSettingTab, Setting, addIcon } from 'obsidian';
import { CardBoardView, VIEW_TYPE_CARD_BOARD } from './view';

export default class CardBoardPlugin extends Plugin {
  settings: {
    data :
      { boardConfigs : (
        { data : { columns : { displayTitle : string; tag : string }[]; completedCount : number; includeOthers : boolean; includeUntagged : boolean; title : string }; tag : "tagBoardConfig" } |
        { data : { completedCount : number; includeUndated : boolean; title : string }; tag : "dateBoardConfig" }
      )[] };
    version : string
  };

  async onload() {
    console.log('loading CardBoard plugin');

    this.registerView(
      VIEW_TYPE_CARD_BOARD,
      (leaf) => new CardBoardView(this, leaf)
    );

    await this.loadSettings();

    addIcon("card-board",
      '<rect x="2" y="2" width="96" height="96" rx="12" ry="12" fill="none" stroke="currentColor" stroke-width="5"></rect>' +
      '<rect x="28" y="28" width="12" height="46" fill="none" stroke="currentColor" stroke-width="5"></rect>' +
      '<rect x="56" y="28" width="12" height="30" fill="none" stroke="currentColor" stroke-width="5"></rect>');

    this.addRibbonIcon('card-board', 'CardBoard Plugin', async () => {
      this.activateView();
    });

    this.addCommand({
      id: "open-card-board-plugin",
      name: "Open Window",
      callback: async () => {
        this.activateView();
      },
    });
  }

  onunload() {
    console.log('unloading CardBoard plugin');
    this.app.workspace.detachLeavesOfType(VIEW_TYPE_CARD_BOARD);
  }

  async activateView() {
    this.app.workspace.detachLeavesOfType(VIEW_TYPE_CARD_BOARD);

    await this.app.workspace.getLeaf(true).setViewState({
      type: VIEW_TYPE_CARD_BOARD,
      active: true,
    });

    this.app.workspace.revealLeaf(
      this.app.workspace.getLeavesOfType(VIEW_TYPE_CARD_BOARD)[0]
    );
  }

  async deactivateView() {
    this.app.workspace.detachLeavesOfType(VIEW_TYPE_CARD_BOARD);
  }

  async loadSettings() {
    this.settings = await this.loadData();
  }

  async saveSettings(
    newSettings: {
      data :
        { boardConfigs : (
          { data : { columns : { displayTitle : string; tag : string }[]; completedCount : number; includeOthers : boolean; includeUntagged : boolean; title : string }; tag : "tagBoardConfig" } |
          { data : { completedCount : number; includeUndated : boolean; title : string }; tag : "dateBoardConfig" }
        )[] };
      version : string
    }
  ) {
    this.settings = newSettings;
    await this.saveData(newSettings);
  }
}
