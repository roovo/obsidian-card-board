# Contributing
I am working on this myself for now; it's my do-some-coding-when-I-have-some-time
project.


## Set up a dev environmant
To get up and running:

```
npm install             # install dependencies
npm run generate        # generate ts type definitions for elm ports
npm run dev             # run dev build
```

## Release Checklist
- elm-review
- elm-test
- elm-coverage
- elm-format
- check production build works
- check with no config (data.json file)
- check with previous config file versions
- is README up to date?
- is TODO up to date?
- test in real vault
- tag the release
- release new version
  - main.js
  - manifest.json (the version in here is what is displayed as the current plugin version
                   in obsidian)
  - style.css
  - release notes
- bump version: manifest.json, package.json


## Run tests
```
npx elm-test
npx chokidar "{src,tests}/**/*.elm" -c "npx elm-test"
npx elm-test ./tests/SpecificFile.elm
```

## Code coverage
```
npm i -g elm-coverage
npx elm-coverage
```

## Formatting elm code
Use elm-format (https://github.com/avh4/elm-format)

```
npm i -g elm-format
```

works best if you set up with editor to format on save.

## Find/remove dead elm code
```
npx elm-review
npx elm-review --fix-all
```

## Generate typescript types for the elm interface (ports)
```
npx chokidar "src/**/*.elm" -c "npm run generate"
```

## Generate example tasks
```
ruby ./scripts/generate_example_tasks.rb path_to_vault
```

This will create an `example_tasks` directory in the vault and put some
example/test files inside it.  If it's already there it will delete
anything already in the directory and put the test files inside.

### Load into an obsidian vault
I have the top level of this project linked to a directory in the
.obsidian/plugins directory in a test vault for development.  I've
set up a hot-key for the `Reload without saving` command to reload
the plugin after changes.

```
ln -s path/to/this/dir path/to/vault/.obsidian/plugins/cardboard
```
