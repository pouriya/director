## Get source
```sh
~ $ git clone https://github.com/Pouriya-Jahanbakhsh/director.git
```


## Compile
```sh
~/director $ make compile
===> Verifying dependencies...
===> Compiling sample
===> Verifying dependencies...
===> Compiling sample_ets
===> Verifying dependencies...
===> Compiling sample_mnesia
===> Verifying dependencies...
===> Compiling director
```


## Use as dependency
##### Rebar3
Put this in deps in rebar.config:
```erlang
{director, "17.9.16"}
```

##### Rebar
Put this in deps in rebar.config:
```erlang
{director, ".*", {git, "https://github.com/Pouriya-Jahanbakhsh/director.git", {tag, "17.9.16"}}}
```

##### Mix
Put this in deps in mix.exs:
```elixir
{:director, "~> 17.9.16"}
```

##### erlang.mk
```make
dep_director = hex 17.9.16
```

## Convert all available markdown files to HTML
After installing python `grip` run:
```sh
~/director $ make md2html
Exporting to ./README.html
Exporting to ./wiki/html/overview.html
Exporting to ./wiki/html/director-behavior.html
Exporting to ./wiki/html/build.html
Exporting to ./examples/sample/README.html
Exporting to ./examples/ETS/README.html
Exporting to ./examples/Mnesia/README.html
```
