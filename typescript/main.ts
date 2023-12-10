import { App, Modal, Notice, Plugin, PluginSettingTab, Setting, addIcon, normalizePath } from 'obsidian';
import { CardBoardView, VIEW_TYPE_CARD_BOARD } from './view';

export type CardBoardPluginSettings = CardBoardPluginSettingsPreV11 | CardBoardPluginSettingsPostV11;

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
  }

  onunload() {
    console.log('unloading CardBoard plugin');
    this.app.workspace.detachLeavesOfType(VIEW_TYPE_CARD_BOARD);
  }


  addCommands() {
    // this.settings?.data?.boardConfigs?.forEach((boardConfig, index) => {
    //   const command = this.addCommand({
    //     id: "open-card-board-plugin-" + index,
    //     name: "Open " + boardConfig.data.title,
    //     callback: async () => {
    //       this.activateView(index);
    //     },
    //   });

    //   this.commandIds.push(command.id);
    // });
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

export type CardBoardPluginSettingsPostV11 = {
  data : {
    boardConfigs : ({
      columns : ({
        tag : "completed"
        data : {
          collapsed : boolean;
          name : string;
          index : number;
          limit : number;
        }
      } | {
        tag : "dated"
        data : {
          collapsed : boolean;
          name : string;
          range : ({
            tag : "after"
            data : number
          } | {
            tag : "before"
            data : number
          } | {
            tag : "between"
            data : {
              from : number
              to : number
            }
          })
        }
      } | {
        tag : "namedTag"
        data : {
          collapsed : boolean;
          name : string;
          tag : string;
        }
      } | {
        tag : "otherTags"
        data : {
          collapsed : boolean;
          name : string;
        }
      } | {
        tag : "undated"
        data : {
          collapsed : boolean;
          name : string;
        }
      } | {
        tag : "untagged"
        data : {
          collapsed : boolean;
          name : string;
        }
      })[];
      filters : ({ data : string; tag : "tagFilter" } | { data : string; tag : "pathFilter" } | { data : string; tag : "fileFilter" })[];
      filterPolarity : "Allow" | "Deny";
      showColumnTags : boolean;
      showFilteredTags : boolean;
      name : string
    })[];
    globalSettings : {
      taskCompletionFormat : "NoCompletion" | "ObsidianCardBoard" | "ObsidianDataview" | "ObsidianTasks";
      defaultColumnNames : {
        today : string;
        tomorrow : string;
        future : string;
        undated : string;
        otherTags : string;
        untagged : string
        completed : string;
      }
      ignoreFileNameDates : boolean;
    }
  };
  version : string
}

type CardBoardPluginSettingsPreV11 = {
  data : {
    boardConfigs : (
      { data : {
          columns : { displayTitle : string; tag : string }[];
          showColumnTags : boolean;
          completedCount : number;
          filters : ({ data : string; tag : "tagFilter" } | { data : string; tag : "pathFilter" } | { data : string; tag : "fileFilter" })[];
          filterPolarity : "Allow" | "Deny";
          showFilteredTags : boolean;
          includeOthers : boolean;
          includeUntagged : boolean;
          title : string
        };
        tag : "tagBoardConfig" }
      | { data : {
          completedCount : number;
          filters : ({ data : string; tag : "tagFilter" } | { data : string; tag : "pathFilter" } | { data : string; tag : "fileFilter" })[];
          filterPolarity : "Allow" | "Deny";
          showFilteredTags : boolean;
          includeUndated : boolean;
          title : string
        };
        tag : "dateBoardConfig"
      }
    )[];
    globalSettings : {
      taskCompletionFormat : "NoCompletion" | "ObsidianCardBoard" | "ObsidianDataview" | "ObsidianTasks";
      columnNames : {
        today : string;
        tomorrow : string;
        future : string;
        undated : string;
        others : string;
        untagged : string
        completed : string;
      }
      ignoreFileNameDates : boolean;
    }
  };
  version : string
}
