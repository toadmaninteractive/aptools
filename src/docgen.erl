-module(docgen).

%% Include files

%% Exported functions

-export([
    gen/1
]).

%% API

gen(Apps) when is_list(Apps) ->
    [ gen(App) || App <- Apps ],
    ok;
gen(App) ->
    Options = [{todo, true}, {packages, false}, {new, true}, {preprocess, true}, {doc_path, ["file:///"++D||D<-edoc_lib:find_doc_dirs()]}, {report_missing_types, true}],
    edoc:application(App, Options).

%% Local functions
