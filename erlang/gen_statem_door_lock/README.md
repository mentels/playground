door_lock
=====

An OTP application

Build
-----

    $ rebar3 compile

## Test in the shell

```rebar3 shell```

```erlang
25> Pid = door_lock:test_correct_code(door_lock_state_based).
Send me (<0.184.0>) 'stop' to stop the door_lock<0.184.0>
The door has been unlocked... Will get locked in 5 s
The door has been locked...
26> Pid ! stop.
stop
```


## Start as an app

```bash
rebar3 shell --apps door_loock --config sys.config
```

Without config `door_lock_state_based` is used with `[1,2,3]` code.

1) Make the app start
rebar3 shell --apps door_loock --config

## TODO
