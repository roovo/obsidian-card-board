import { App, Modal, Notice, Plugin, PluginSettingTab, Setting } from 'obsidian';
import { KanbanView } from './view';

interface MyPluginSettings {
	mySetting: string;
}

const DEFAULT_SETTINGS: MyPluginSettings = {
	mySetting: 'default'
}

export default class KanbanPlugin extends Plugin {
	settings: MyPluginSettings;

	async onload() {
    console.log('loading kanban plugin');

    await this.loadSettings();

    this.addRibbonIcon('dice', 'Kanban Plugin', async () => {
      const leaf = this.app.workspace.getLeaf(!(this.app.workspace.activeLeaf.view.getViewType() == 'empty'));
      const view = new KanbanView(this.app.vault, leaf);
      await leaf.open(view);
    });
  }

  onunload() {
    console.log('unloading kanban plugin');
  }

  async loadSettings() {
    await this.loadData();
  }
}
