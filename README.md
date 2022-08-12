# nliu.net

This is the repo where I host the source for my [personal site](https://nliu.net).

Posts are written in a combination of Markdown and Literate Haskell, and are compiled into HTML at build time using [Hakyll](https://github.com/jaspervdj/hakyll).

You can find the raw static site [here](https://github.com/dreamsmasher/dreamsmasher.github.io).

## Building

The build process assumes you're also based and nix-pilled.

You might want to enable [nix-direnv](https://github.com/nix-community/nix-direnv) for easier development, which caches your nix-shell environment.

```bash
# build the site generator executable
nix --experimental-features 'nix-command flakes' build -vL

# run a clean build using the site generator
./result/bin/nliu-exe rebuild

# OPTIONAL: start a server for live update during development
./result/bin/nliu-exe clean && ./result/bin/nliu-exe watch
```

