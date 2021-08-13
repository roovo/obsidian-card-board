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
npm run dev             # run dev build
npx elm-test --watch    # run (elm) tests
```

So your elm code is nicely formatted, use elm-format
(https://github.com/avh4/elm-format) and set up with editor to format on save.

```
npm i -g elm-format
```
