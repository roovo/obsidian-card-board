import { Filter } from "./types"

export class FileFilter {
  private excludes: string[];

  constructor(filters: Filter[]) {
    this.buildExcludes(filters);
  }

  filter(file: { path: string }, callback: Function) {
    if (!(this.isInExcludes([file.path]))) {
      callback(file);
    }
  }

  private isInExcludes(paths: string[]) {
    return this.excludes.some(item => paths.includes(item));
  }

  private buildExcludes(filters: Filter[]) {
    this.excludes = filters.map(filter => filter.data);
  }
}
