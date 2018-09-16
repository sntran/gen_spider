-type name() :: atom() | {'local', term()} | {'global', term()} | {'via', module(), term()}.
-type spider() :: pid() | name() | {atom(), node()}.
-type on_start() :: {'ok', pid()} | 'ignore' |  {'error', {'already_started', pid()} | term()}.
-type url() :: binary().
-type option() ::
  {'name', name()}
  | {'start_urls', list()}
  | {'allowed_domains', [url()]}
  | {'delay', non_neg_integer()}
  | {'interval', non_neg_integer()}.
-type request() :: [{'url', url()}
                    | {'method', method()}
                    | {'headers', [header()]}
                    | {'payload', term()}].
-type method() :: 'head' | 'get' | 'put' | 'post' | 'trace' | 'options' | 'delete' | 'patch'.
-type header() :: {binary(), term()}.
-type response() :: #{'url' := url(),
                    'status' := non_neg_integer(),
                    'headers' := [header()],
                    'body' := binary(),
                    'request' := request()}.
-type text() :: binary().
-type selector() :: binary().
-type reason() :: 'normal' | 'shutdown' | {'shutdown', term()} | term().
-type version() :: term | {'down', version()}.
