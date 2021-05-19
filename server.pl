:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_error)).
:- use_module(library(yaml)).
:- use_module(library(http/http_log)).

% Server

:- http_handler(root(Path), redir_request(Path), [method(_Method), methods([get])]).

server(Port) :-
  http_server(http_dispatch, [port(Port)]).

% Redirect exists
redir_request(Page, Request) :-
  request_subdomain(Request, Subdomain),
  redirection(Subdomain, Page, Url),
  http_redirect(moved, Url, Request).

% Fallback path
redir_request(Page, _Request) :-
  format('Status: 404~n'),
  format('Content-type: text/plain~n~n'),
  format('No redirect for page ~a~n', Page).

request_subdomain(Request, Subdomain) :-
  member(host(Host), Request),
  domain_subdomain(Host, Subdomain).

domain_subdomain(Domain, Subdomain) :- 
  split_string(Domain, ".", "", [Subdomain, _|_]).

% redirection mappings

links_file_path(Path) :- 
  getenv("HEADLIGHTS_LINKS_PATH", Path) ; Path = "./links.yaml".

raw_links(Text) :- 
  links_file_path(Path), 
  read_file_to_string(Path, Text, []).

links_data(Yaml) :- 
  raw_links(Raw), 
  yaml_read(string(Raw), Yaml).

redirection(SubDomainV, PathV, Url) :-
  value_atom(SubDomainV, SubDomain),
  value_atom(PathV, Path),
  links_data(Yaml),
  Url = Yaml.get(SubDomain).get(Path).

% Utils

value_atom(A, A) :- atom(A).
value_atom(V, A) :- atom_string(A, V).


% vim:ft=prolog
