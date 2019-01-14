## Notes on the [`error_logger` integration](https://github.com/erlang-lager/lager#error-logger-integration)

* in the console the `error_logger` logs are formatted the SASL way; the lagger formatting appears just in the log files
* to configure the treshold `{error_logger_hwm, Num}` lager option is used to indicate the max no of `error_logger` msgs / sec
* why we still see the messages in the console of the format:
  ```
  =INFO REPORT==== 14-Jan-2019::12:03:24.039157 ===
  [<0.3319.0>] [119] Log message
  ```
  while there's just the lager handler:
  ```
  21> error_logger:which_report_handlers().
  [error_logger_lager_h]
  ```
  * looks like it only happens if we call `error_logger:info_msg/X` from a process, not from the shell
  * IO Leader? What happens if the code is not run from the shell directly?!

## RabbitmQ and file_backend

```erlang
`sbin/rabbitmqctl eval "[lager:log(info, [], \"ala\") || _ <- lists:seq(1,10000)]."
```

output:

```
 lager_file_backend dropped 4980 messages in the last second that exceeded the limit of 50 messages/sec
```

is the limit per file or the entire backend?

## General lager question

How is the default high_water_mark set? Can it be changed? How?
