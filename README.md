# Obsidian CardBoard Plugin
An [Obsidian](https://obsidian.md/) plugin to make working with tasks a pleasure
(hopefully anyway).

- Keep your tasks wherever you like in your vault.
- View them on kanban style boards.
- Use regular tasks and subtasks.
- Two board types supported:
  - Date based (with daily/periodic notes support).
  - Tag based (use `#tags` to define your boards).

## New
- Per-board filtering using file, path, and tag filters.
- Front matter tag support.

![date based board screenshot](/images/dateBoard.png?raw=true)

## Installation
Please install via the regular Community Plugins setting tab within Obsidian.

If you want to keep up to date with any Beta releases then add it to the v.handy
[obsidian42 BRAT plugin](https://github.com/TfTHacker/obsidian42-brat).

## Use
When installed, you can launch the plugin:

- using the icon in the app ribbon (see below), or
- using the Command Pallete command (which allows you to open a specific board).

![app ribbon icon](/images/ribbonIcon.png?raw=true)

If you have no boards defined, you should get a dialog asking you to add a new
board.  There are 2 types of board:

- **Date based**: looks like the main screenshot above.
- **Tag based**: uses tags to define the columns (you need to include tags on
  your tasks or in front matter for this to work).

Name and configure your boards and you are good to go.

## Cards
Any task in your vault can appear as a card in a column on a board.  In order to
do this, it must:

- Be in a markdown file.
- Not be indented.
- Have the format: `- [ ] Task title`.

What appears on the card depends on what your task looks like:

- Anything that is indented under a task will appear in the body of the task.
  - Indented tasks will appear as subtasks (all subtasks are grouped together).
  - Indented text will appear as notes.
- `#tags` in front matter or on the line of the task (or any subtasks) will
  appear at the top of the card.
- Due date (if given) will appear at the bottom of the card.

So, if you had the following in one of your markdown files:

```
- [ ] run erands @due(2021-10-30)
  - [x] do shopping #town
  - [ ] wash car #home/outside
  - [ ] cook dinner #home/kitchen

  perhaps I should look up some [[example_tasks/recipes|recipes]] first

  - [ ] do something with a long title that will truncate when displayed
  - [ ] go to bed
```

It will look something like this on a card on your board:

![example card](/images/card.png?raw=true)

#### Marking a task as complete
If you mark an item as complete on the board it will be marked as completed in the markdown
(and vice-versa).  If you mark as complete on the board, a completion timestamp is appended
to the task:

```
- [x] Task title @completed(2021-10-30T13:57:48)
```

If you have subtasks and the parent task is tagged as an _autocomplete_ task then the main
task will be marked as complete when you tick off the final subtask:

```
- [ ] Task title @autocomplete(true)
  - [ ] Do this first
  - [ ] Do this next
  - [ ] Finally do this and you are done
```


### Deleting a task
You can delete a task using the trash icon on the card.  This will not actually delete
the task from your vault, it simply surrounds it with markdown `<del>` tags:

```
<del>- [x] Task title</del>
```

### Editing tasks (and hover preview)
Click on the edit icon to open the file containing the task.  Cmd (or Ctrl on windows)
hover over the icon for the normal Obsidian hover preview.


### Column ordering
The current behaviour for the different columns is:

- **Completed**: has the most recently completed at the top (assuming they were
  marked as complete using the checkbox on the board).
- **Future**, **Today**, **Tags**: these are sorted by due date and then
  alphabetically within this.
- other columns are sorted alphabetically.

I am not convinced that this is the best strategy so this may well change in a future release.

### Customising Tags
If you like to apply custom styles to your tags, this is now possible
(thank you @darthmachina).  Tags have a class that reflects the tag name.
So if you have a tag `#foo/bar` you will be able to style it with your favorite
color for foo/bars (which just has to be HotPink) using:

```css
.card-board-view .card-board-card-tag.tag-foo-bar > span.cm-hashtag {
  background-color: HotPink;
  color: DimGrey;
}
```

## Date boards
You will get the best out of these if you are using the (core) Daily Notes
or the (community) Periodic Notes plugins, as any tasks you place on a daily
note will be assigned to the day of the note.

You can also assign a date to any task using the format:

```
- [ ] My task @due(2021-10-31)
```

### Overdue tasks
These will appear in the `Today` column above any  any tasks that are actually
due today.

The idea being that it will get steadily more annoying to see what you were planning
to do today if you have a lot of incomplete tasks from previous days, (hopefully)
encouraging you to do something about them; like do them or move them to a future
date if you want to schedule them later.


## Tag boards
If you give your tasks tags, you can use these to set up a tag-board.  So if you
have the tags `#project1/backlog`, `#project1/triaged`, `project1/blocked`, `#project1/doing`,
you can define a board that shows tasks tagged with these in separate columns:

![tag board settingx](/images/tagBoardSettings.png?raw=true)

### Subtags
If you specify a tag with a trailing `/` then the column will contain all subtags of the tag.

## Front Matter Tags
If you want to give all the tasks on a page the same tag, you can put it in the
page front matter:

```
---
tags: [ project1 ]
---

# Project 1

- [ ] this task will automatically have a project1 tag
```

## Board Filters
You can filter which tasks appear on each board in the board settings.  There are 3
types of filter you can use: file, path, and #tags (including front matter tags).  You can
use any combination of these on a per-board basis.

You can also choose whether to use the filters as an allow or a deny list.

## Settings
Plugin settings are accessible from the plugin view itself, via the settings icon
above the board to the left of the tabs.  You can:

- Create new boards (using the + icon next to _BOARDS_).
- Configure your boards.
- Delete any boards you no longer need.

## Limitations
- Might not work that great on large vaults (as it parses all markdown files at startup).
- Might not be great on mobile (see previous, plus I haven't made the interface mobile
  friendly - yet).

## Alternatives
If the way that this works isn't for for you, there are plenty of other fabulous
plugins you can use for task management in Obsidian.
[Kanban](https://github.com/mgmeyers/obsidian-kanban),
[Checklist](https://github.com/delashum/obsidian-checklist-plugin), and
[Tasks](https://github.com/schemar/obsidian-tasks) are the most popular. There are
others too, see the list on the wonderful
[roundup site](https://www.obsidianroundup.org/plugins/).


## Contributing
I am working on this myself for now; it's my do-some-coding-when-I-have-some-time
project.  However, if you want to mess around with the code then see
[contributing doc](CONTRIBUTING.md) for more info on getting a dev environment set
up and running.

If you have any thoughts, ideas, bugs n stuff:

- **Bugs/suggestions/feature requests** - [github issues](https://github.com/roovo/obsidian-card-board/issues).
- **What's being worked on and up next?** - [CardBoard Dev](https://github.com/users/roovo/projects/2)
- **Questions/discussions** - [github discussions](https://github.com/roovo/obsidian-card-board/discussions)

