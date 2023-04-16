% Declare the module "server" with the exported predicate "server/1"
:- module(server, [server/1]).

% Import necessary libraries for the server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_client)).
:- use_module(library(yaml)).

% Define the server predicate with the http_handler to handle requests
:- http_handler(root(Path), redir_request(Path), []).

% server/1 predicate starts the server on the specified Port
server(Port) :-
  http_server(http_dispatch, [port(Port)]).

% redir_request/2 predicate handles the case where a redirect exists
redir_request(Page, Request) :-
  % log request
  request_subdomain(Request, Subdomain),
  redirection(Subdomain, Page, Url),
  http_redirect(moved, Url, Request).

% redir_request/2 predicate handles the fallback case when no redirect is found
redir_request(Page, _Request) :-
  format('Status: 404~n'),
  format('Content-type: text/plain~n~n'),
  format('No redirect for page ~a~n', Page).

% request_subdomain/2 predicate extracts the subdomain from the Request.
% If there is no subdomain, i.e. in `elkowar.dev`, then the subdomain returned is `@`.
request_subdomain(Request, Subdomain) :-
  member(host(Host), Request),
  domain_subdomain(Host, Subdomain).

domain_parts(Domain, Parts, Count) :-
  split_string(Domain, ".", "", Parts),
  length(Parts, Count).

% domain_subdomain/2 predicate splits the domain to obtain the subdomain
% If there is no subdomain, i.e. in `elkowar.dev`, then the subdomain returned is ``.
domain_subdomain(Domain, "") :- 
  domain_parts(Domain, _Parts, 2).
domain_subdomain(Domain, Subdomain) :-
  domain_parts(Domain, Parts, Count),
  Count > 2,
  % all but the last two elements of the "Parts" list, concatenated with a dot
  append(Prefix, [_Last, _SecondLast], Parts),
  atomic_list_concat(Prefix, ".", Subdomain).


% links_data_url/1 predicate retrieves the URL of the links data from the environment variable
links_data_url(Url) :- getenv("HEADLIGHTS_LINKS_URL", Url).

% links_data/1 predicate fetches the links data from the URL and parses it as YAML
links_data(Data) :-
  links_data_url(Url),
  http_get(Url, Raw, []),
  yaml_read(string(Raw), Data).

% redirection/3 predicate retrieves the URL for the specified SubDomain and Path from the links data
redirection(SubDomainV, PathV, Url) :-
  value_atom(SubDomainV, SubDomain),
  value_atom(PathV, Path),
  links_data(Yaml),
  Url = Yaml.get(SubDomain).get(Path).

% value_atom/2 predicate converts a value to an atom if it is not already an atom
value_atom(A, A) :- atom(A).
value_atom(V, A) :- atom_string(A, V).

% vim:ft=prolog
