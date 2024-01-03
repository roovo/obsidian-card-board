import { Filter } from "./types"

export class FileFilter {
  private excludedPaths: string[];

  constructor(filters: Filter[]) {
    this.buildExcludes(filters);
  }

  filter(file: { path: string }, callback: Function) {
    if (!(this.isExcluded(this.pathsToMatch(file.path)))) {
      callback(file);
    }
  }

  private buildExcludes(filters: Filter[]) {
    this.excludedPaths = filters
      .filter(f => f.tag != "tagFilter")
      .map(filter => filter.data);
  }

  private isExcluded(paths: string[]) {
    return this.excludedPaths.some(item => paths.includes(item));
  }

  private parentFolder(path: string): string {
    return path.split("/").slice(0, -1).join("/");
  }

  private pathsToMatch(path: string): string[] {
    let remainingPath: string = path;
    let paths: string[] = [path];

    while (remainingPath.length > 0) {
      remainingPath = this.parentFolder(remainingPath);
      paths.push(remainingPath);
    }

    return paths.slice(0, -1);
  }
}
