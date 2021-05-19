
:- multifile http:location/3.
:- dynamic http:location/3.


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(yaml)).


:- http_handler(root(Path), 
                redir_request(Path), 
                [method(_Method), methods([get])]
               ).

server(Port) :-
  http_server(http_dispatch, [port(Port)]).


raw_yaml("
  dots: https://github.com/elkowar/dots-of-war
  eww: https://github.com/elkowar/eww
").
my_yaml(Yaml) :- raw_yaml(Raw), yaml_read(string(Raw), Yaml).

redirection(Path, Url) :-
  my_yaml(Yaml),
  get_dict(Path, Yaml, Url).

redir_request(Page, Req) :-
  redirection(Page, Url),
  http_redirect(moved, Url, Req).

redir_request(Path, _Req) :-
  atom_string(Path, PathStr),
  format('Content-type: text/plain~n~n'),
  format('What is this "~w" you\'re talking about??~n', PathStr).


% vim:ft=prolog
