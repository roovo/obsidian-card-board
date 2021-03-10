import * as obsidianTodo from './todo';

test('parses a simple uncompleted markdown TODO', async () => {
  const markdown = "- [ ] I need doing";
  const todo = obsidianTodo.parse(markdown);

  expect(todo).toEqual([{title: "I need doing", isComplete: false}]);
});

test('parses a simple completed markdown TODO', async () => {
  const markdown = "- [x] I have been done";
  const todo = obsidianTodo.parse(markdown);

  expect(todo).toEqual([{title: "I have been done", isComplete: true}]);
});

test('parses an indented markdown TODO', async () => {
  const markdown = "    - [ ] I need doing";
  const todo = obsidianTodo.parse(markdown);

  expect(todo).toEqual([{title: "I need doing", isComplete: false}]);
});

test('parses multiple uncompleted markdown TODOs', async () => {
  const markdown = `- [ ] thing one
  - [x] thing two
  - [ ] thing three`;
  const todo = obsidianTodo.parse(markdown);

  expect(todo).toEqual([
    {title: "thing one", isComplete: false},
    {title: "thing two", isComplete: true},
    {title: "thing three", isComplete: false},
  ]);
});

test('parses string with no valid todos', async () => {
  const markdown = "nothing to do here";
  const todo = obsidianTodo.parse(markdown);

  expect(todo).toEqual([ ]);
});
