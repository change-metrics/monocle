# Contributing

## Understanding the design choices

Follow the [Architectural Decision Records](doc/adr/index.md) to
understand the choices made by the project.

## To deploy from source code

To use `docker-compose` from source code, use the
`docker-compose.yml.dev` file instead of the image based one:

```ShellSession
$ ln -sf docker-compose.yml.dev docker-compose.yml
```

## Reloading code

This section explains how you can hack the Monocle code. The idea is to use
the docker deployment to avoid complex development methods.

After changes, simply run the following command. docker-compose will
rebuild and re-spawn the changed containers:

```ShellSession
$ docker-compose up -d --build
```

### auto-reloading the web UI code

To have your code automatically reloaded on each change, just run it
outside the container:

```ShellSession
$ docker-compose stop web
$ cd web
$ npm install
$ npm start
```

## Git hooks

Before submitting a [Pull Request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request-from-a-fork),
make sure to configure the git hooks locally to avoid proposing broken code
or not well formatted code.

### pre-push

To be sure to push correct branches, you have to configure the
`pre-push` git hook by creating `.git/hooks/pre-push` with the
following content:

```Shell
#!/bin/bash

exec ./contrib/pre-push "$@"
```

and making it executable with `chmod +x .git/hooks/pre-push`.

### pre-commit

To be sure to have correctly formatted code, enable the `pre-commit`
git hook to reformat your code by creating `.git/hooks/pre-commit`
with the following content:

```Shell
#!/bin/bash

exec ./contrib/pre-commit "$@"
```

and making it executable with `chmod +x .git/hooks/pre-commit`.

## Contributing a new driver

Please refer to the file `dummy/change.py` that contain a Dummy driver and
some comments to help you get started.
