erlpkg
=====

A `rebar3` plugin for Windows MSI and Linux RPM builder for erlang applications

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { erlpkg, ".*", {git, "git@host:user/erlpkg.git", {tag, "2.0.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 erlpkg
    ===> Fetching erlpkg
    ===> Compiling erlpkg
    <Plugin Output>
