import { FileFilter } from "../typescript/fileFilter"
import { Filter } from "../typescript/types"

describe('fileFilter', () => {

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
});
