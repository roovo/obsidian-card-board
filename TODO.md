- do I want to rename Panel -> Board ??


# Task Formats
- https://github.com/schemar/obsidian-tasks
- https://logseq.github.io/#/page/tasks%20%26%20todos
- https://blacksmithgu.github.io/obsidian-dataview/data-annotation/
- https://forum.obsidian.md/t/task-management-devs-add-date-format-standard/26464

# UI Improvements
- I parse 'foo:w' as a tag whereas obsidian only recognises the foo part.
- do I want to warn the user that I couldn't read settings at startup?
  are there any sensible options I can give them if I do?
-- do I want the last board who's settings I've edited to the one shown when closing settings?
- allow boards to be ordered (in settings somehow - perhaps use this as a first exploration
  into drag and drop?)
- when jumping to the todo using the edit link perhaps I could highlight the complete
  taskItem block (if is has indented content).  Will have to explore the setEphemeralState
  code to see if I can do this using line numbers or whether I need to track characters..
- mobile support?
  - what is it like performance-wise?
  - prob need to use CSS grid more than I am (specially on the settings modal)
- do I want to keep the tabbar in view when horizontal scrolling? (prob yes)
- "spinner" whilst loading tasks (perhaps like in the sidebar when doing searches)
- does openLinkText use setSelection to highlight the selected todo?
  if so, I can do better as by default obsidian doesn't include all
  indented stuff under a todo item as being in the block
- confirm dialog before task deletion
- keyboard navigation
- search for card by title/content
- resizing columns
- what to do about due dates for subtasks
- undo buffer for toggling completion
- could/should I use some taskpaper tags:
    @defer(date) - defer until date, e.g. 2016-04-19 5pm or next Thursday -3d
    @estimate(time span) - time estimate, e.g. 2h for 2 hours or 3w for 3 weeks.
    @flagged - present when an item is flagged
    @parallel(bool) - whether children are parallel (true) or sequential (false)
    @repeat-method(method) - the repeat method: fixed, start-after-completion, or due-after-completion
    @repeat-rule(rule) - an ICS repeat rule (see RFC244557), e.g. FREQ=WEEKLY;INTERVAL=1
    taskpaper grammer: https://support.hogbaysoftware.com/t/taskpaper-bnf-grammar/4002/3
- edit in place via popup

# Board Improvements
- Tagboard: have separate Other and Untagged columns
- do I want to sort undated (or other in tagged board) column by the mod date of the containg file?
- do I want a way of flagging cards?
- could make column ordering more efficient (e.g. TagBoard completed tasks)
- should I have an option to set colors for tags?
- context menu to set due date to today
- don't display all completed tasks - can get quite long!
- filter cards on board (e.g. by tag)
- sort order for columns?

# Card Improvements
- specify format for people cards
  - if on line and they contain an image then put the pic on the card
  - support multiple people
- might be cleaner when generating markdown for display on a card to remove the wrapping <p> tag
  intesad of trying to style it's effect away using css
- right click on interal link
- right click on external link
- when clicking the edit button place the cursor at the line of the todo
  have tried to do this using setCursor and not go it to work so far
- display on card - done date

# Theme Compatibility
- Firefly Theme: why is the text so big?
- get working with tabbed view plugin

# Board Types
- have a subtag board that uses a root tag then subtags to define columns
- reverse subtag: #class1/week1, #class2/week1 (you could specify week1 to get both class 1 & 2 on a board)
- eisenhower matrix view
- #3 define a board from the contents of a file with columns set by the headings on a page

# Drag n Drop
- on a tagboard when drag-dropping allow alt-drag to duplicate the card
  - so maintaining the tag in the column being dragged from as well as getting the new
- multiselect - for drag/drop and context menu operations
- drag and drop
  - into today, tomorrow, and done columns
  - within column to change ordering
- would need a date picker for dragging into (e.g.) future
  suggestion from discord: koala
  31 Oct at 07:52
  https://github.com/TfTHacker/obsidian42-jump-to-date
  https://github.com/liamcain/obsidian-calendar-ui
  For a date picker.

# Misc
- https://allcontributors.org/docs/en/overview
- how small can I make the compliled js?
  https://discourse.elm-lang.org/t/what-i-ve-learned-about-minifying-elm-code/7632
  look at esbuild
- put the target in dist
  - put the static sources in an assets dir and copy them into dist on build too- BadInputFromTypeScript -> I don't do anything if I can something bad from
- TaskItem.tags should really be a Set
  - would need to add a Set.Extra module otherwise the code will be less readable
- work out how I will handle changes to the settings file format
- Could I write a worker that keeps an eye on what is being edited and adds
  a completion timestamp when it is done?
- Settings to:
  - allow/deny directories/files
  - set max title lines
- I see that elm-ts-json now has a pipleline decoder - should switch to this as
  it makes decoders easier to read
- in the update for SettingsUpdated I am re-writing all markdown content and updating
  the hover for all edit buttons.  Can I be smarter?  Only matters really if performance
  issues.
- can I use github runners to build?
- run elm review
- where to use fuzz testing
- review awful typescript code!
- return something to elm if I fail to re-write a TODO due to the line having changed so I can
  let the user know why nothing has happened
- better parsing errors? - https://discourse.elm-lang.org/t/newline-and-indentation-issues-in-elm-parser/4869
- look into issue fixed in commit bca367 : TODO - why does this work....
  - can I get parser to always work when there is no "\n" on the end of the input
- if it is slow parsing vaults then see if I can speed it up by
  dropping backtrackable in the TaskItems parser.

api option for preview view so when given a block reference is still shows the whole document
but scrolls the block into view and highlights it.  When this is done I can use this when
hovering over the edit button to show the details of the todo in the original doc.  At the moment
if you do this, it will only show the single line of the todo and not any subtasks or content.

https://forum.obsidian.md/t/see-context-in-hover-preview-of-block-reference/10232

# Done

- make README.md a proper readme (and have a separate contributing/dev page?)
- add a licence
- remove jest as I am not using it
- run elm review
- check it works with the default setup for the daily notes plugin
- check it works in a virgin vault with no plugins
- bump settings version to 0.1.0
- handle renames - make sure I cope with both directory and file renames
- persist settings in the plugin when they are updated
- use the excellent advice at:  https://marcus.se.net/obsidian-plugin-docs/guides/custom-views
  for guidance on creating views
- plugin settings
- display multiple boards
- I'm often passing Time.Posix and Time.Zone around together -> make a type
- don't store functions on the model (board is stored as a function at the moment)
- don't use elm-typescript-interop (as it won't install so I can't build project from new)
- leave column title at the top when scrolling cards
- California Coast Theme: why are the ticks in checboxes
  (in the Completed column) not in the checkboxes?
- UI: style task title (color) as if it is a h1 (can I make it a h1)
- make @autodone @autocomplete and make all done's completed's?
- do I want to use text-muted for the due date?
- highlight overdue tasks, (flagged tasks - not implemented yet) and those due today
- dateboard: secondary sort should be card title
- option to include done and undated for DateBoard
- always show tags as tag pills whatever the theme
- define boards by #tags
- bugfix: if parent taks has no tags then the child tags aren't picked up
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
