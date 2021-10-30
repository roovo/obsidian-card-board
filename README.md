## Development

I have the main linked to a directory in the .obsidian/plugins directory
in a test vault for development (where you can set up a hot-key for the
`Reload without saving` command to reload the plugin after changes):

```
ln -s path/to/this/dir path/to/vault/.obsidian/plugins/cardboard
```

To get up and running:

```
npm install             # install dependencies
npm run gnerate         # generate ts type definitions for elm ports
npm run dev             # run dev build
npx elm-test --watch    # run (elm) tests
```

So your elm code is nicely formatted, use elm-format
(https://github.com/avh4/elm-format) and set up with editor to format on save.

```
npm i -g elm-format
```

### Find/remove dead elm code
npx elm-review --template jfmengels/elm-review-unused/example
npx elm-review --template jfmengels/elm-review-unused/example --fix-all

### Keep elm and ts in sync
```
npx chokidar "src/**/*.elm" -c "npm run generate"
```

### Run tests
```
npx elm-test
npx chokidar "{src,tests}/**/*.elm" -c "npx elm-test"
```

### Generate example tasks

```
ruby ./scripts/generate_example_tasks.rb path_to_vault
```

This will creat an `example_tasks` directory in the vault and put some
example/test files inside it.  If it's already there it will delete
anything already in the directory and put the test files inside.
