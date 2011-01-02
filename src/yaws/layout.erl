-module(layout).
-export([page/2]).

page(Page, Title) ->
    [
    {html, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"},
    {ehtml,
        {html, [{xmlns, "http://www.w3.org/1999/xhtml"}], [
            {head, [], [
                {title, [], Title},
                {link, [{rel, "stylesheet"}, {type, "text/css"}, {href, "test.css"}]}
            ]},
            {body, [], [
                {'div', [{class, "header"}], [
                    {a, [{href, "torrent_list.yaws"}], "Torrent list"}
                ]},
                Page,
                {'div', [{class, "footer"}], [
                    "Fultracker"
                ]}
            ]}
        ]}
    }
    ].
