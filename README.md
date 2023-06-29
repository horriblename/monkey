# Monkey

An implementation of the monkey interpreter, as described in <https://interpreterbook.com>.

## Future improvements

- proper UTF-8 support
- better errors (include line/col number in errors)
- garbage collector! (right now all variables/objects live in `Rc` hell)
- rework environment system
  - environments are cloned pretty much everywhere, even where they should be sharing the same env
