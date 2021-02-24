import * as obsidianTodo from './todo';

test('parses a simple uncompleted markdown TODO', async () => {
  const markdown = "- [ ] I need doing";
  const todo = obsidianTodo.parse(markdown);

  expect(todo).toEqual({title: "I need doing", isComplete: false})
});

test('parses a simple completed markdown TODO', async () => {
  const markdown = "- [x] I have been done";
  const todo = obsidianTodo.parse(markdown);

  expect(todo).toEqual({title: "I have been done", isComplete: true})
});

test('parses an indented markdown TODO', async () => {
  const markdown = "    - [ ] I need doing";
  const todo = obsidianTodo.parse(markdown);

  expect(todo).toEqual({title: "I need doing", isComplete: false})
});
