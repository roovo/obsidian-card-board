// https://codemix.com/opaque-types-in-javascript/
type Opaque<K, T> = T & { __TYPE__: K };

const matcher = /- \[(.)\]\s(.*)/g

export type Todo = Opaque<'Todo', { }>;

type $Todo = Todo & {
  title: string,
  isComplete: boolean
};

function buildTodo([ , mark, title]: [string , string, string]): Todo {
  const todoFields = {
    title: title,
    isComplete: mark === 'x'
  };

  return (todoFields as $Todo) as Todo;
}

export function parse(markdown: string): Todo[] {
  return [...markdown.matchAll(matcher)].map(buildTodo);
}
