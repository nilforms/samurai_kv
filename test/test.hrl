%% HTTP content type headers
-define(header_content_type_url_encoded, [{<<"content-type">>, "application/x-www-form-urlencoded"}]).
-define(content_type_json, {<<"content-type">>, <<"application/json">>}).

-define(url_encode(List), uri_string:compose_query(List)).