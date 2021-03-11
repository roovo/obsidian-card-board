import {ItemView, WorkspaceLeaf} from 'obsidian';
import { Elm } from '../src/Main';
import KanbanPlugin from './main';

export class KanbanView extends ItemView {
  plugin: KanbanPlugin;

  constructor(plugin: KanbanPlugin, leaf: WorkspaceLeaf) {
    super(leaf);
    this.plugin = plugin;
  }

  async onOpen() {
    const elmDiv = document.createElement('div');
    elmDiv.id = "elm-node";
    this.containerEl.children[1].appendChild(elmDiv);  // let app = Elm.Main.init({ flags: null });
    Elm.Main.init({
        // node: document.getElementById("elm-node")
        node: elmDiv
    })
  }

  getDisplayText(): string {
    return 'Kanban';
  }

  getViewType(): string {
    return 'Kanban';
  }
}
