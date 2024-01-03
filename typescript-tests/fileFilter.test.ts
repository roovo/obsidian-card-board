import { FileFilter } from "../typescript/fileFilter"
import { Filter } from "../typescript/types"

describe('fileFilter', () => {
  describe('filter', () => {
    test('calls the callback if there are no filters', async () => {
      const filters: Filter[] = [];
      const testFile          = { path : 'foo.md' }
      const mockCallback      = jest.fn(f => f.path);
      const fileFilter        = new FileFilter(filters);

      fileFilter.filter(testFile, mockCallback);

      expect(mockCallback.mock.calls).toHaveLength(1);
      expect(mockCallback.mock.results[0].value).toEqual('foo.md');
    });

    test('calls the callback if there are non-mmatching filters', async () => {
      const filters: Filter[] = [{ tag : "fileFilter", data : "bar.md"  }];
      const testFile          = { path : 'foo.md' }
      const mockCallback      = jest.fn(f => f.path);
      const fileFilter        = new FileFilter(filters);

      fileFilter.filter(testFile, mockCallback);

      expect(mockCallback.mock.calls).toHaveLength(1);
      expect(mockCallback.mock.results[0].value).toEqual('foo.md');
    });

    test('DOES NOT CALL the callback if there is a matching file filter', async () => {
      const filters: Filter[] = [{ tag : "fileFilter", data : "foo.md"  }];
      const testFile          = { path : 'foo.md' }
      const mockCallback      = jest.fn(f => f.path);
      const fileFilter        = new FileFilter(filters);

      fileFilter.filter(testFile, mockCallback);

      expect(mockCallback.mock.calls).toHaveLength(0);
    });

    test('DOES NOT CALL the callback if there is a matching path filter', async () => {
      const filters: Filter[] = [{ tag : "pathFilter", data : "aFolder"  }];
      const testFile          = { path : 'aFolder/foo.md' }
      const mockCallback      = jest.fn(f => f.path);
      const fileFilter        = new FileFilter(filters);

      fileFilter.filter(testFile, mockCallback);

      expect(mockCallback.mock.calls).toHaveLength(0);
    });

    test('DOES NOT CALL the callback if there is a deeper matching path filter', async () => {
      const filters: Filter[] = [{ tag : "pathFilter", data : "aFolder/bFolder"  }];
      const testFile          = { path : 'aFolder/bFolder/cFolder/foo.md' }
      const mockCallback      = jest.fn(f => f.path);
      const fileFilter        = new FileFilter(filters);

      fileFilter.filter(testFile, mockCallback);

      expect(mockCallback.mock.calls).toHaveLength(0);
    });

    test('ignores the contents of tagFilters', async () => {
      const filters: Filter[] = [{ tag : "tagFilter", data : "aFolder/bFolder"  }];
      const testFile          = { path : 'aFolder/bFolder/cFolder/foo.md' }
      const mockCallback      = jest.fn(f => f.path);
      const fileFilter        = new FileFilter(filters);

      fileFilter.filter(testFile, mockCallback);

      expect(mockCallback.mock.calls).toHaveLength(1);
      expect(mockCallback.mock.results[0].value).toEqual('aFolder/bFolder/cFolder/foo.md');
    });

    test('calls the callback if the file is in a parent folder of a pathFilter', async () => {
      const filters: Filter[] = [{ tag : "pathFilter", data : "aFolder/bFolder"  }];
      const testFile          = { path : 'aFolder/foo.md' }
      const mockCallback      = jest.fn(f => f.path);
      const fileFilter        = new FileFilter(filters);

      fileFilter.filter(testFile, mockCallback);

      expect(mockCallback.mock.calls).toHaveLength(1);
      expect(mockCallback.mock.results[0].value).toEqual('aFolder/foo.md');
    });
  });

  describe('isAllowed', () => {
    test('returns true if there are no filters', async () => {
      const filters: Filter[] = [];
      const testPath          = 'foo.md';
      const fileFilter        = new FileFilter(filters);

      expect(fileFilter.isAllowed(testPath)).toEqual(true);
    });

    test('return true if there are non-mmatching filters', async () => {
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
  });
});
