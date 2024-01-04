import { App, Modal, Notice, Plugin, PluginSettingTab, Setting, addIcon, normalizePath } from 'obsidian';
import { CardBoardView, VIEW_TYPE_CARD_BOARD } from './view';
import { CardBoardPluginSettings, CardBoardPluginSettingsPostV11 } from './types';
import * as fs from 'fs';

export default class CardBoardPlugin extends Plugin {
  private commandIds: string[] = [];
  settings: CardBoardPluginSettings;

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

    this.addRibbonIcon('card-board', 'CardBoard', async () => {
      this.activateView(0);
    });

    this.addCommands();


    this.startSettigsWatch();
  }

  startSettigsWatch() {
    const that = this;

    // @ts-ignore
    const pathToSettings = normalizePath(this.app.vault.adapter.basePath + "/" + this.app.vault.configDir + "/plugins/card-board/data.json");

    // @ts-ignore
    this.app.vault.adapter.fs.watchFile(pathToSettings, { interval: 1007 }, (current : fs.Stats, previous : fs.Stats) => {
      console.log('settings file changed');

      const leaves = this.app.workspace.getLeavesOfType(VIEW_TYPE_CARD_BOARD);

      for (const leaf of leaves) {
        if (leaf.view instanceof CardBoardView) {
          leaf.view.handleUpdateSettings(that.settings);
        }
      }

    });
  }

  onunload() {
    console.log('unloading CardBoard plugin');
    this.app.workspace.detachLeavesOfType(VIEW_TYPE_CARD_BOARD);
  }


  addCommands() {
    this.settings?.data?.boardConfigs?.forEach((boardConfig, index) => {
      const config : any = boardConfig;
      var boardName : string;

      if (config.hasOwnProperty('data')) {
        boardName = config.data.title;
      } else {
        boardName = config.name;
      }

      const command = this.addCommand({
        id: "open-card-board-plugin-" + index,
        name: "Open " + boardName,
        callback: async () => {
          this.activateView(index);
        },
      });

      this.commandIds.push(command.id);
    });
  }


  removeCommands() {
    for (const commandId of this.commandIds) {
      // @ts-ignore
      this.app.commands.removeCommand(commandId);
    }
    this.commandIds = [];
  }

  async activateView(index: number) {
    this.app.workspace.detachLeavesOfType(VIEW_TYPE_CARD_BOARD);

    await this.app.workspace.getLeaf(true).setViewState({
      type: VIEW_TYPE_CARD_BOARD,
      active: true,
    });

    const leaf = this.app.workspace.getLeavesOfType(VIEW_TYPE_CARD_BOARD)[0];

    if (leaf.view instanceof CardBoardView) {
      leaf.view.currentBoardIndex(index);
    }

    this.app.workspace.revealLeaf(leaf);
  }

  async deactivateView() {
    this.app.workspace.detachLeavesOfType(VIEW_TYPE_CARD_BOARD);
  }

  async loadSettings() {
    this.settings = await this.loadData();
  }

  async saveSettings( newSettings: CardBoardPluginSettingsPostV11) {
    await this.backupOldVersion(this.settings?.version, newSettings.version);

    this.removeCommands();
    this.addCommands();
    this.settings = newSettings;
    await this.saveData(newSettings);
  }

  async backupOldVersion(oldVersion: string | null, newVersion: string) {
    if (oldVersion && (oldVersion != newVersion)) {
      const pathToSettings = normalizePath(this.app.vault.configDir + "/plugins/card-board/data.json");
      const pathToSavedSettings = normalizePath(this.app.vault.configDir + "/plugins/card-board/data." + oldVersion + ".json");

      if (await this.app.vault.adapter.exists(pathToSavedSettings)) {
        await this.app.vault.adapter.remove(pathToSavedSettings);
      }
      this.app.vault.adapter.copy(pathToSettings, pathToSavedSettings);
    }
  }
}
