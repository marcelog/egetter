## About
A simple erlang application to manage downloading. Will choose a proxy and a
user agent at random from the given lists.

## Configuration example

```
{egetter, [
  {user_agents, "/etc/egetter/priv/agents.txt"},
  {proxies, "/etc/egetter/proxies.txt"},
  {request_timeout, 5000},
  {ibrowse_options, [{max_sessions, 20}]}
]}
```
