// WARNING: Do not manually modify this file. It was generated using:
// https://github.com/dillonkearns/elm-typescript-interop
// Type definitions for Elm ports

export namespace Elm {
  namespace Main {
    export interface App {
      ports: {
        deleteTodo: {
          subscribe(callback: (data: { filePath: string; lineNumber: number; originalText: string }) => void): void
        }
        displayTodoMarkdown: {
          subscribe(callback: (data: { filePath: string; todoMarkdown: { id: string; markdown: string }[] }) => void): void
        }
        editTodo: {
          subscribe(callback: (data: { filePath: string; lineNumber: number; originalText: string }) => void): void
        }
        updateTodos: {
          subscribe(callback: (data: { filePath: string; todos: { lineNumber: number; originalText: string; newText: string }[] }) => void): void
        }
        fileAdded: {
          send(data: { filePath: string; fileDate: string | null; fileContents: string }): void
        }
        fileDeleted: {
          send(data: string): void
        }
        fileUpdated: {
          send(data: { filePath: string; fileDate: string | null; fileContents: string }): void
        }
      };
    }
    export function init(options: {
      node?: HTMLElement | null;
      flags: { folder: string; format: string };
    }): Elm.Main.App;
  }
}