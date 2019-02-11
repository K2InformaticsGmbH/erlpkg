erlpkg
=====

Windows MSI and Linux RPM installer packager for erlang

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { erlpkg, ".*", {git, "git@host:user/erlpkg.git", {tag, "1.0.1"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 erlpkg
    ===> Fetching erlpkg
    ===> Compiling erlpkg
    <Plugin Output>
