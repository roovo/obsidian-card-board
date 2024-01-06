import { Filter } from "./types"

export class FileFilter {
  private excludedPaths: string[];

  constructor(filters: Filter[]) {
    this.buildExcludes(filters);
  }

  isAllowed(path: string): boolean {
    for (let elem of this.excludedPaths) {
      if (path.includes(elem)) return false;
    }
    return true;
  }

  private buildExcludes(filters: Filter[]) {
    this.excludedPaths = filters
      .filter(f => f.tag != "tagFilter")
      .map(f => f.tag == "fileFilter" ? f.data : f.data + "/");
  }
}
