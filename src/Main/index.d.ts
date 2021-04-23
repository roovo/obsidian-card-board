// WARNING: Do not manually modify this file. It was generated using:
// https://github.com/dillonkearns/elm-typescript-interop
// Type definitions for Elm ports

export namespace Elm {
  namespace Main {
    export interface App {
      ports: {
        deleteTodo: {
          subscribe(callback: (data: { filePath: string; lineNumber: number; title: string }) => void): void
        }
        editTodo: {
          subscribe(callback: (data: { filePath: string; lineNumber: number; title: string }) => void): void
        }
        rewriteTodo: {
          subscribe(callback: (data: { filePath: string; lineNumber: number; title: string; newText: string }) => void): void
        }
        fileUpdated: {
          send(data: { filePath: string; fileDate: string | null; fileContents: string }): void
        }
        fileAdded: {
          send(data: { filePath: string; fileDate: string | null; fileContents: string }): void
        }
        fileDeleted: {
          send(data: string): void
        }
      };
    }
    export function init(options: {
      node?: HTMLElement | null;
      flags: { folder: string; format: string };
    }): Elm.Main.App;
  }
}