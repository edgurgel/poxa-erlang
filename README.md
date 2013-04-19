[![Build Status](https://travis-ci.org/edgurgel/pusherl.png?branch=master)](https://travis-ci.org/edgurgel/pusherl)

# Pusherl

Open Pusher implementation compatible with Pusher libraries. It's designed to be used as a single registered app with id, secret and key defined on start.


## Features

* Public channels;
* Private channels;

## TODO

* SSL on websocket and REST api;
* Presence channels;
* Mimic pusher error codes;
* Integration test using pusher-js or other client library;
* Web hooks;
* Specify types signature to functions and use dyalizer to check them;
* Exclude clients based on socket_id.
* Split in rebar.config.test and rebar.config


## Typical usage

Pusherl is a standalone erlang server implementation of the Pusher protocol.

Clone this repository

Run

```
$ make deps compile
```
The default configuration is:

* Port: 8080
* App id: 'app_id'
* App key: 'app_key'
* App secret: 'secret'

You can set these values using this command:

```
$ erl -pa ebin deps/*/ebin -pusherl_api port 9090 -pusherl_api app_id '<<"12345">>' -pusherl_api app_key '<<"key-12345">>' -pusherl_api app_secret '<<"secret6789">>' -s  pusherl_api
```

Or you can setup a configuration file like this:

test.config

```erlang
[{pusherl_api, [{port, 8080},
                {app_id, <<"12345">>},
                {app_key, <<"key-12345">>},
                {app_secret, <<"secret6789">>}]}].
```

And run:

```
$ erl -pa ebin deps/*/ebin -config test -s  pusherl_api
```

## Release

If you just want to run an executable, follow these instructions:

TODO


## Your application

If you are using the pusher-gem:

```ruby
Pusher.host   = 'localhost'
Pusher.port   = 8080
```

```javascript
Pusher.host    = 'localhost'
Pusher.ws_port = 8080
```


## Implementation

Pusherl uses [gproc](https://github.com/uwiger/gproc) extensively to register websocket connections as channels. So, when a client subscribes for channel 'example-channel', the websocket connection (which is a erlang process) is "tagged" as **{pusher, example-channel}**. So when a pusher event is triggered on the 'example-channel', every websocket matching the tag receives the event.

## Contributing

If you'd like to hack on Pusherl, start by forking my repo on Github.

You need [Erlang](http://www.erlang.org) and [rebar](https://github.com/basho/rebar). We are using Erlang R15B03, but you may try use it with other versions.

Dependencies can be fetched running:

```
$ make deps
```

To compile:

```
$ make
```

The test suite used is the simple EUnit and [meck](http://github.com/eproxus/meck) to mock stuff. You can run the tests:

```
$ make test
```

Pull requests are greatly appreciated.

## Pusher

Pusher is an excellent service and you should use it on production.

