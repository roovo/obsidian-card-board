// https://codemix.com/opaque-types-in-javascript/

type Opaque<K, T> = T & { __TYPE__: K };

export type Todo = Opaque<'Todo', { }>;

export function todo(input: { title: string, isComplete: boolean }) {
  return (input as $Todo) as Todo;
}

// internal - not accessible outside this file
type $Todo = Todo & {
  title: string,
  isComplete: boolean
};
