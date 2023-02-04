#!/usr/bin/env ruby

require "fileutils"
require "date"

if ARGV.length != 1
  puts "You must give the path to the vault where you want the examples created"
end

target_directory = File.join(ARGV[0], "example_tasks")

puts target_directory

unless File.directory?(target_directory)
  Dir.mkdir(target_directory)
end

FileUtils.rm_rf("#{target_directory}/.", secure: true)

todays_tasks = File.join(target_directory, "tasks_for_today.md")

File.open(todays_tasks, "w") do |file|
  file.write """
# Tasks For Today

- [ ] run erands @due(#{Date.today.strftime("%Y-%m-%d")})
  - [x] do shopping #town
  - [ ] wash car #home/outside
  - [ ] cook dinner #home/kitchen

  perhaps I should look up some [[example_tasks/recipes|recipes]] first

  - [ ] do something with a long title that will truncate when displayed
  - [ ] go to bed

- [x] already got out of bed this morning @due(#{Date.today.strftime("%Y-%m-%d")}) @completed(#{(Date.today).strftime("%Y-%m-%dT%H:%M:%S")})
- [ ] مادرم را به خرید ببر @due(#{Date.today.strftime("%Y-%m-%d")}) #خانه/آشپزخانه
- [ ] something with #foobar an RTL non-sub tag @due(#{Date.today.strftime("%Y-%m-%d")}) #خانه
	foo bar baz #خانه/آشپزخانه #one two
	and another line #foobar something else
"""
end

tomorrows_tasks = File.join(target_directory, "tasks_for_tomorrow.md")

File.open(tomorrows_tasks, "w") do |file|
  file.write """
# Tasks For Tomorrow

- [ ] book meal at fancy restaurant @due(#{(Date.today + 1).strftime("%Y-%m-%d")}) #Status/🔥Active
- [ ] meditate @due(#{(Date.today + 1).strftime("%Y-%m-%d")}) #wellbeing #home
"""
end

tomorrows_daily_note_tasks = File.join(target_directory, "#{(Date.today + 1).strftime("%Y-%m-%d")}.md")

File.open(tomorrows_daily_note_tasks, "w") do |file|
  file.write """
# Daily note For Tomorrow

- [ ] daily note task for tomorrow with no inline date
- [ ] daily note task for tomorrow with inline date set to none @due(none)
- [ ] daily note task for tomorrow with an over-ride for the day after @due(#{(Date.today + 2).strftime("%Y-%m-%d")})
"""
end

daily_note_template = File.join(target_directory, "daily_note_template.md")

File.open(daily_note_template, "w") do |file|
  file.write """
# Daily note template
"""
end

future_tasks = File.join(target_directory, "future_tasks.md")

File.open(future_tasks, "w") do |file|
  file.write """
# Tasks For the Future

- [ ] next week thing @due(#{(Date.today + 7).strftime("%Y-%m-%d")})
- [ ] day after tomorrow thing @due(#{(Date.today + 2).strftime("%Y-%m-%d")})
"""
end

undated_tasks = File.join(target_directory, "undated_tasks.md")

File.open(undated_tasks, "w") do |file|
  file.write """
# Undated things to do

- [ ] this is a task with @autocomplete set @autocomplete(true)
  - [ ] subtask1
  - [ ] subtask 2

- [x] got out of bed last week @completed(#{(Date.today - 7).strftime("%Y-%m-%dT%H:%M:%S")})
- [x] got out of bed yesterday @completed(#{(Date.today - 1).strftime("%Y-%m-%dT%H:%M:%S")})

- [ ] an undated task that links to [[tasks_for_today|other file]]
- [ ] another undated task (but with a block id) ^34d21

- [ ] #important and this is a task tagged as important #projects/project1 #status/backlog
- [ ] and this is a task NOT tagged as important #projects/project1 #status/doing

- [ ] a Dataview task with a due date [due:: #{(Date.today).strftime("%Y-%m-%d")}]
- [x] a completed Dataview task with a due date [due:: #{(Date.today - 1).strftime("%Y-%m-%d")}] [completion:: #{(Date.today - 1).strftime("%Y-%m-%d")}]

- [ ] a Tasks task with just a due date 📅 #{(Date.today).strftime("%Y-%m-%d")}
- [ ] a Tasks task with just a scheduled date ⏳ #{(Date.today - 1).strftime("%Y-%m-%d")}
- [ ] a Tasks task with a due and a scheduled date ⏳ #{(Date.today - 2).strftime("%Y-%m-%d")} 📅 #{(Date.today - 1).strftime("%Y-%m-%d")}
- [x] a completed Tasks task with just a due date 📅 #{(Date.today - 1).strftime("%Y-%m-%d")} ✅ #{(Date.today - 1).strftime("%Y-%m-%d")}

- [x] a completed task with no @completed
- [x] a completed task with all varieties of completion tags @completed(#{(Date.today).strftime("%Y-%m-%dT%H:%M:%S")}) ✅ #{(Date.today - 1).strftime("%Y-%m-%d")} [completion:: #{(Date.today - 1).strftime("%Y-%m-%d")}]
- [x] thought a bit about yoga #wellbeing
- [ ] this task has a pretty long title so it won't all fit in the task title on the card
  it also has a nice long block of task notes underneath it

  including some code:

  ```elm
  main : Program Flags Model Msg
  main =
      Browser.element
          { init = init
          , update = update
          , subscriptions = subscriptions
          , view = view
          }
  ```
"""

colab_tasks = File.join(target_directory, "colab.md")

File.open(colab_tasks, "w") do |file|
  file.write """
# Colaboration examples

- [ ] need to ask #people/fred if he can loan me is dinosaur @due(#{Date.today.strftime("%Y-%m-%d")})

- [ ] decorate my cave @due(#{(Date.today + 7).strftime("%Y-%m-%d")})
  - [x] ask #people/wilma to do a cave painting @due(#{(Date.today).strftime("%Y-%m-%d")})
  - [ ] borrow design book from #people/barney
  - [ ] check everyone can make the painting party @due(#{(Date.today + 4).strftime("%Y-%m-%d")})

"""
end
end
