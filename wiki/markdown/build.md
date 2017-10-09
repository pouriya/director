## Get source
```sh
$ git clone https://github.com/Pouriya-Jahanbakhsh/director.git
```


## Get compiled
From [Release 17.9.16](https://github.com/Pouriya-Jahanbakhsh/director/releases/tag/17.9.16) Download **lib.tar.gz**



## Compile
```sh
$ rebar compile
```
or
```sh
$ rebar3 compile
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
