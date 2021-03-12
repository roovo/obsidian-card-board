import {ItemView, Vault, WorkspaceLeaf} from 'obsidian';
import { Elm } from '../src/Main';
import KanbanPlugin from './main';

export class KanbanView extends ItemView {
  private vault: Vault;

  constructor(vault: Vault, leaf: WorkspaceLeaf) {
    super(leaf);
    this.vault = vault;
  }

  async onOpen() {
    const elmDiv = document.createElement('div');
    elmDiv.id = "elm-node";
    this.containerEl.children[1].appendChild(elmDiv);

    const app = Elm.Main.init({
      node: elmDiv,
      flags: "Hello from flags"
    })

    const markdownFiles = this.vault.getMarkdownFiles();
    for (const file of markdownFiles) {
      const fileContents = await this.vault.cachedRead(file);
      app.ports.dataForElm.send({
        tag: "MarkdownToParse",
        data: {
          filePath: file.path,
          fileContents: fileContents
        }
      });
    }
  }

  getDisplayText(): string {
    return 'Kanban';
  }

  getViewType(): string {
    return 'Kanban';
  }
}
