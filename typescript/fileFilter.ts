import { Filter } from "./types"

export class FileFilter {
  private excludes: string[];

  constructor(filters: Filter[]) {
    this.buildExcludes(filters);
  }

  filter(file: { path: string }, callback: Function) {
    if (!(this.isInExcludes(this.pathsToMatch(file.path)))) {
      callback(file);
    }
  }

  private isInExcludes(paths: string[]) {
    return this.excludes.some(item => paths.includes(item));
  }

  private buildExcludes(filters: Filter[]) {
    this.excludes = filters.map(filter => filter.data);
  }

  private pathsToMatch(path: string): string[] {
    let remainingPath: string = path;
    let paths: string[] = [path];

    while (remainingPath.length > 0) {
      remainingPath = this.getParentFolder(remainingPath);
      paths.push(remainingPath);
    }

    return paths.slice(0, -1);
  }

  private getParentFolder(path: string): string {
    return path.split("/").slice(0, -1).join("/");
  }
}
