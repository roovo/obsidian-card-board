// https://codemix.com/opaque-types-in-javascript/
type Opaque<K, T> = T & { __TYPE__: K };

const matcher = /- \[(.)\]\s(.*)/

export type Todo = Opaque<'Todo', { }>;

type $Todo = Todo & {
  title: string,
  isComplete: boolean
};

export function parse(markdown: string): Todo {
  const [ , mark, title] = markdown.match(matcher);
  return ({title: title, isComplete: mark === 'x'} as $Todo) as Todo;
}
