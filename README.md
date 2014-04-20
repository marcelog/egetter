## About
A simple erlang application to manage downloading. Will choose a proxy and a
user agent at random from the given lists.

## How to use it

 * Start the main supervisor [egetter_sup](https://github.com/marcelog/egetter/blob/master/src/egetter_sup.erl) from your own supervisor
tree.

 * Configure the application, here's a sample:
```
{egetter, [
  {user_agents, "/etc/egetter/priv/agents.txt"},
  {proxies, "/etc/egetter/proxies.txt"},
  {request_timeout, 5000},
  {ibrowse_options, [{max_sessions, 20}]}
]}
```

 * You will need a list of proxies and a list of user agents. There are examples
 in the [priv directory](https://github.com/marcelog/egetter/tree/master/priv)
 
 * Call **egetter:req/1** passing in a url (a string). You will get a proplist with:
   * {headers, [{string(), string()}]}
   * {body, binary()}