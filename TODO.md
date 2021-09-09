- define boards by #tags and/or #tags/subtags
  - don't store functions on the model tho (baord is stored as a function at the moment)
- make README.md a proper readme (and have a separate contributing/dev page?)
- plugin settings
  - allow/deny directories/files
  - max title lines

- UI: style task title (color) as if it is a h1 (can I make it a h1)
- "spinner" whilst loading tasks (perhaps like in the sidebar when doing searches)
- does openLinkText use setSelection to highlight the selected todo?
  if so, I can do better as by default obsidian doesn't include all
  indented stuff under a todo item as being in the block
- define boards by headings on a page
- confirm dialog before task deletion
- don't display all completed tasks - can get quite long!
- handle renames - make sure I cope with both directory and file renames
- when clicking the edit button place the cursor at the line of the todo
  have tried to do this using setCursor and not go it to work so far
- do I want to always display tags as tag pills?
  if I do, I could allow them to be colored via settings
- run elm review
- specify format for people cards
  - if on line and they contain an image then put the pic on the card
  - support multiple people
- might be cleaner when generating markdown for display on a card to remove the wrapping <p> tag
  intesad of trying to style it's effect away using css
- right click on interal link
- right click on external link
- what to do about due dates for subtasks
- keyboard navigation
- sort order for columns? (perhaps store the order information in front matter)
- drag and drop -> into today, tomorrow, and done columns
- search for card by title/content
- filter cards on board (e.g. by tag)
- undo buffer for toggling completion
- where to use fuzz testing
- return something to elm if I fail to re-write a TODO due to the line having changed so I can
  let the user know why nothing has happened
- look into issue fixed in commit bca367 : TODO - why does this work....
  - can I get parser to always work when there is no "\n" on the end of the input
- better parsing errors? - https://discourse.elm-lang.org/t/newline-and-indentation-issues-in-elm-parser/4869
- resizing columns
- display on card - done date
- edit in place via popup
- if it is slow parsing vaults then see if I can speed it up by
  dropping backtrackable in the TaskItems parser.
- review awful typescript code!

- could/should I use some taskpaper tags:

    @defer(date) - defer until date, e.g. 2016-04-19 5pm or next Thursday -3d
    @estimate(time span) - time estimate, e.g. 2h for 2 hours or 3w for 3 weeks.
    @flagged - present when an item is flagged
    @parallel(bool) - whether children are parallel (true) or sequential (false)
    @repeat-method(method) - the repeat method: fixed, start-after-completion, or due-after-completion
    @repeat-rule(rule) - an ICS repeat rule (see RFC244557), e.g. FREQ=WEEKLY;INTERVAL=1

    taskpaper grammer: https://support.hogbaysoftware.com/t/taskpaper-bnf-grammar/4002/3

- put the target in dist
  - put the static sources in an assets dir and copy them into dist on build too

# Need help
api option for preview view so when given a block reference is still shows the whole document
but scrolls the block into view and highlights it.  When this is done I can use this when
hovering over the edit button to show the details of the todo in the original doc.  At the moment
if you do this, it will only show the single line of the todo and not any subtasks or content.

https://forum.obsidian.md/t/see-context-in-hover-preview-of-block-reference/10232

# Done

- use a kanban icon for the sidebar
- add a timestamp to done for better ordering
  ensure that just a date is valid too
  update the time every second
  add a test for ParserHelper.timeParser as it will not parse a valid time string
- order done tasks by done date
- order future tasks by due date
- script to generate some files containing example/test todos
- ensure it still works if there is no daily notes plugin installed (just uses @due tags)
  - it does!
- make it so the board actually fits vertically in the display view without needing scrollbars
- use elm review
- bring up preview of the card when hovering over the edit icon
- when click on edit link on card go to the right place on the page
- bugs
  - why is the checkbox smaller for the main task than subtasks in the test vault?
  - Q: why is the task in the test vault not parsing?
    A: cause it is using tabs not spaces
- why don't links in card titles work? (and why don't they show a linked card on hover) - now implemented
- bugfix: why did the checkin for dispaying notes make all the Done tasks show as one-liners?
- indented text as markdown task notes
  - including invalid/incomplete subtasks
  - including blank lines
- include any subtask tags in the card tag list
- bug - tags aren't written on re-write of a TaskItem (after toggling)
- support @autodone(bool) taskpaper tag
- show indented tasks as subtasks
- TaskItem - use a record internally (for ease of understanding)
- only display 2 lines of task title
- render card title as markdown
  - pass filePath with card title to ts for use in RenderMarkdown call
  - render titles in ts when card content is edited inside Obsidian
  - render titles in ts when card content is edited outside Obsidian
- check the date periodically (say every minute) so the board updates around midnight
- display on card
  - tags
  - due date
- #-tag support
- ensure "- [ ] foo @done(2020-01-01)x" doesn't extract the @done tag
- add @due(date) - overrides the day of the daily note it appears on
- on completion add a @done(date) to the task
  - use the original source text the TaskItem was derived from to verify before re-writing
- do I want to keep trailing spaces when parsing TaskItem title?
  no need as toString removes them
- loose all reference to kanban - call it card board
- make it switch to the kanban window when ribbon icon clicked
- make it look nicer!
- on edit in file, jump to and highlight the relevant line
  - https://codepen.io/Ratia/pen/gwNNgX
- click on some icon to jump to location in file
- use a trash icon for delete
- Delete todos
- Implement mark as done (add Pending Completion to type)
- Show checkbox to mark as done
- Ordering when reading files and displaying tasks
  - specially for today, future, and done columns
  - this is relevant when someone is updating a file as I don't want the tasks from that file
    to move around the column they are in.
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
