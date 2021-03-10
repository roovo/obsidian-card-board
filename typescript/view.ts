import {ItemView, WorkspaceLeaf} from 'obsidian';
import KanbanPlugin from './main';

export class KanbanView extends ItemView {
  plugin: KanbanPlugin;

  constructor(plugin: KanbanPlugin, leaf: WorkspaceLeaf) {
    super(leaf);
    this.plugin = plugin;
  }

  async onOpen() {
    // set up the view
  }

  getDisplayText(): string {
    return 'Kanban';
  }

  getViewType(): string {
    return 'Kanban';
  }
}
