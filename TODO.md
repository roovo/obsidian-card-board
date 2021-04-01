- Ordering when reading files and displaying tasks
  - specially for today, future, and done columns
  - this is relevant when someone is updating a file as I don't want the tasks from that file
    to move around the column they are in.
- Show checkbox to mark as done
- Implement mark as done (add Pending Completion to type)
- render card title as markdown
- click on card to jump to location in file
- handle renames - make sure I cope with both directory and file renames
- edit in place via popup
- show a trash icon on a card and click it to delete (with confirmation)
- drag and drop -> into today, tomorrow, and done columns
- do I want to keep trailing spaces when parsing TaskItem title?
- specify directories/files to ignore (allow and deny lists?)
- light/dark/user installed themes
- make it switch to the kanban window when ribbon icon clicked
- make sure there can only be one Kanban window no matter how many times the icon is clicked
- if it is slow parsing vaults then see if I can speed it up by
  dropping backtrackable in the TaskItems parser.

- can I get parser to always work when there is no "\n" on the end of the input
- put the target in dist
  - put the static sources in an assets dir and copy them into dist on build too
- elm review
- elm debugger
- why does the compilation fail with  TS2307: Cannot find module '../src/Main' or its corresponding type declarations.


# Done

- Update board as files are updated in the app vault
  - updated, created, deleted
- add some testing around adding, updating, removing tasks from a list of TaskItems
- add some tests for filtering for taskItems not from a given path
- bug
  parsing fails if there is a task list that contains a task prefix with nothing after it, so:

- [ ] bar task
- [ ] this is a new task that I am adding right now :)
- [ ]<only a single space here>

  will not parse the task list!!
- Work out why some cards are not appearing in tomorrow and future?
- Add some tests for card filtering
- filter into 2 groups: incomplete and done
- display on simple 2 colum board: todo & done
- build todos from markdown content
- factor out common parser helpers
- pass pages into elm for parsing for todo
- elm typescript interop
- elm format
- put an elm application in the Kanban window
