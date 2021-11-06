# Contributing
No idea how this will work at the moment, but if you fancy trying to
build the plugin then something along the following lines should work.

## Development
I have the top level of this project linked to a directory in the
.obsidian/plugins directory in a test vault for development.  I've
set up a hot-key for the `Reload without saving` command to reload
the plugin after changes):

```
ln -s path/to/this/dir path/to/vault/.obsidian/plugins/cardboard
```

To get up and running:

```
npm install             # install dependencies
npm run generate         # generate ts type definitions for elm ports
npm run dev             # run dev build
```

### Run tests
```
npx elm-test
npx chokidar "{src,tests}/**/*.elm" -c "npx elm-test"
```

### Formatting elm code
Use elm-format (https://github.com/avh4/elm-format)

```
npm i -g elm-format
```

works best if you set up with editor to format on save.

### Find/remove dead elm code
npx elm-review --template jfmengels/elm-review-unused/example
npx elm-review --template jfmengels/elm-review-unused/example --fix-all


### Generate typescript types for the elm interface (ports)
```
npx chokidar "src/**/*.elm" -c "npm run generate"
```

### Generate example tasks

```
ruby ./scripts/generate_example_tasks.rb path_to_vault
```

This will create an `example_tasks` directory in the vault and put some
example/test files inside it.  If it's already there it will delete
anything already in the directory and put the test files inside.
