import { FileFilter } from "../typescript/fileFilter"
import { Filter } from "../typescript/types"

describe('fileFilter', () => {
  describe('isAllowed', () => {
    test('returns true if there are no filters', async () => {
      const filters: Filter[] = [];
      const testPath          = 'foo.md';
      const fileFilter        = new FileFilter(filters);

      expect(fileFilter.isAllowed(testPath)).toEqual(true);
    });

    test('return true if there are non-matching filters', async () => {
      const filters: Filter[] = [{ tag : "fileFilter", data : "bar.md"  }];
      const testPath          = 'foo.md';
      const fileFilter        = new FileFilter(filters);

      expect(fileFilter.isAllowed(testPath)).toEqual(true);
    });

    test('returns false if there is a matching file filter', async () => {
      const filters: Filter[] = [{ tag : "fileFilter", data : "foo.md"  }];
      const testPath          = 'foo.md';
      const fileFilter        = new FileFilter(filters);

      expect(fileFilter.isAllowed(testPath)).toEqual(false);
    });

    test('raturns false if there is a matching path filter', async () => {
      const filters: Filter[] = [{ tag : "pathFilter", data : "aFolder"  }];
      const testPath          = 'aFolder/foo.md';
      const fileFilter        = new FileFilter(filters);

      expect(fileFilter.isAllowed(testPath)).toEqual(false);
    });

    test('raturns false if there is a deeper matching path filter', async () => {
      const filters: Filter[] = [{ tag : "pathFilter", data : "aFolder/bFolder"  }];
      const testPath          = 'aFolder/bFolder/cFolder/foo.md';
      const fileFilter        = new FileFilter(filters);

      expect(fileFilter.isAllowed(testPath)).toEqual(false);
    });

    test('raturns true if there is a file with the name of the path in a path filter', async () => {
      const filters: Filter[] = [{ tag : "pathFilter", data : "aFolder/bFolder"  }];
      const testPath          = 'aFolder/bFolder';
      const fileFilter        = new FileFilter(filters);

      expect(fileFilter.isAllowed(testPath)).toEqual(true);
    });

    test('raturns true if there is a file in a parent path of a path filter', async () => {
      const filters: Filter[] = [{ tag : "pathFilter", data : "aFolder/bFolder"  }];
      const testPath          = 'aFolder/foo.md';
      const fileFilter        = new FileFilter(filters);

      expect(fileFilter.isAllowed(testPath)).toEqual(true);
    });

    test('ignores tag filters', async () => {
      const filters: Filter[] = [{ tag : "tagFilter", data : "foo.md"  }];
      const testPath          = 'foo.md';
      const fileFilter        = new FileFilter(filters);

      expect(fileFilter.isAllowed(testPath)).toEqual(true);
    });
  });
});
