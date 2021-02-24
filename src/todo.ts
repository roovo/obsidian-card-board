// https://codemix.com/opaque-types-in-javascript/
type Opaque<K, T> = T & { __TYPE__: K };

const matcher = /- \[(.)\]\s(.*)/g

export type Todo = Opaque<'Todo', { }>;

type $Todo = Todo & {
  title: string,
  isComplete: boolean
};

function buildTodo(matchArray: RegExpMatchArray): Todo {
  const todoFields = {
    title: matchArray[2],
    isComplete: matchArray[1] === 'x'
  };

  return (todoFields as $Todo) as Todo;
}

export function parse(markdown: string): Todo[] {
  const matchArrays = [...markdown.matchAll(matcher)];

  return matchArrays.map(buildTodo);
}
