// WARNING: Do not manually modify this file. It was generated using:
// https://github.com/dillonkearns/elm-typescript-interop
// Type definitions for Elm ports

export namespace Elm {
  namespace Main {
    export interface App {
      ports: {
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