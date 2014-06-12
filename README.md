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
  {proxies, "/etc/egetter/proxies.txt"}
]}
```

 * You will need a list of proxies and a list of user agents. There are examples
 in the [priv directory](https://github.com/marcelog/egetter/tree/master/priv)
 
 * Call **egetter:req/1** passing in a list of options. You will get a proplist with:
   * {headers, [{string(), string()}]}
   * {status, string()}
   * {body, binary()}

 * Available options:
   * {url, string()}
   * {timeout, pos_integer()}
   * {headers, [{string()|atom(), string()}]}
   * {body, binary()}
   * {method, get | post | put | delete | head | options}
   * {follow_redirect, true | false} (defaults to false)
   * {ibrowse_options, [{atom(), term()}]}
   * {use_proxy, true | false} (defaults to false)
   * {save_to, string()}