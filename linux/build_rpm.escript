#!/usr/bin/env escript
%% -*- mode: erlang -*-
%% ex: ft=erlang
%%! -smp enable -sname build_rpm -mnesia debug verbose
-include_lib("kernel/include/file.hrl").
-include("../common.hrl").

main(main) ->
    ScriptPath = filename:dirname(escript:script_name()),
    CmnLibMod = filename:join([ScriptPath, "..", "common"]),
    case {file:read_file_info(CmnLibMod++".erl"),
          file:read_file_info(CmnLibMod++".beam")} of
        {{ok, #file_info{mtime = M1}},
         {ok, #file_info{mtime = M2}}} when M1 =< M2 -> ok;

        _ ->
            case compile:file(CmnLibMod,
                              [{outdir, filename:join(ScriptPath,"..")},
                               report]) of
                {ok, common} -> ?L("common compile");
                error ->
                    ?L("common compile failed"),
                    error(common_compile_error)
            end
    end,

    put(config, #config{platform="linux"}),

    ?L("loading common library ~p", [CmnLibMod]),
    case code:load_abs(CmnLibMod) of
        {error, What} -> error(What);
        {module, common} -> common:main(ScriptPath)
    end,
    
    C = get(config),
    ?L("--------------------------------------------------------------------------------"),
    ?L("packaging ~p", [C#config.app]),
    ?L("--------------------------------------------------------------------------------"),
    ?L("name        : ~s", [C#config.pkgName]),
    ?L("version     : ~s", [C#config.version]),
    ?L("description : ~s", [C#config.desc]),
    ?L("company     : ~s", [C#config.pkgCompany]),
    ?L("comment     : ~s", [C#config.pkgComment]),
    ?L("priv dirs   : ~p", [C#config.privFolders]),
    ?L("app root    : ~s", [C#config.topDir]),
    ?L("tmp src     : ~s", [C#config.tmpSrcDir]),
    ?L("RPM path    : ~s", [C#config.buildPath]),
    ?L("rebar       : ~s", [C#config.rebar]),
    ?L("--------------------------------------------------------------------------------"),
    build_rpm();
main(Opts) when length(Opts) > 0 ->
    case lists:member("-v", Opts) of
        true -> put(verbose, true);
        false -> put(verbose, false)
    end,
    case lists:member("-sb", Opts) of
        true -> put(skip_build, true);
        false -> put(skip_build, false)
    end,
    main(main);
main([]) ->
    put(verbose, false),
    put(skip_build, false),
    main(main).

build_rpm() ->
    C = get(config),
    % Build a source tarball
    case get(skip_build) of
        false ->
            [begin
                 Dir = filename:join(C#config.buildPath, D),
                 ?OSCMD("rm -rf "++Dir),
                 ok = file:make_dir(Dir)
             end || D <- ["BUILD", "BUILDROOT", "RPMS", "SOURCES", "SPECS", "SRPMS"]],
            build_sources(),
            ?L("source tree built");
        _ ->
            ?L("skipped source tree build")
    end,

    % Build Spec
    make_spec(),

    ?L("Building rpm, this may take a while..."),
    common:run_port(?OSCMD("which rpmbuild"),
                    ["-vv", "-ba",
                     filename:join(
                       filename:join(C#config.buildPath, "SPECS"),
                       C#config.app++".spec")]).

build_sources() ->
    C = get(config),
    ProjDir = C#config.topDir,
    RpmSources = filename:join(C#config.buildPath, "SOURCES"),
    Version = C#config.version,
    RootDir = filename:join(RpmSources, C#config.app++"-"++Version),
    ?OSCMD("rm -rf "++RootDir),
    ok = file:make_dir(RootDir),
    [begin
         Src = filename:join(ProjDir,F),
         Dst = filename:join(RootDir,F),
         case file:copy(Src,Dst) of
             {ok, _} -> ok;
             Error ->
                 ?L("Error copying ~s to ~s", [Src,Dst]),
                 error(Error)
         end
    end || F <- ["rebar.config", "LICENSE", "README.md", "RELEASE-MPRO.md"]],
    ?OSCMD("cp -L `which rebar` "++RootDir),
    copy_folder(ProjDir, RootDir, ["include"], "*.*"),
    copy_folder(ProjDir, RootDir, ["src"], "*.*"),
    copy_folder(ProjDir, RootDir, ["src", "smpp_parser"], "*.*"),
    copy_folder(ProjDir, RootDir, ["src", "ucp_parser"], "*.*"),
    copy_folder(ProjDir, RootDir, ["docs"], "*.*"),
    copy_folder(ProjDir, RootDir, ["rel"], "*.*"),
    copy_folder(ProjDir, RootDir, ["rel", "files"], "*"),
    copy_folder(ProjDir, RootDir, ["rel", "rpmbuild"], "*.*"),

    Priv = filename:join(RootDir, "priv"),
    ok = file:make_dir(Priv),
    copy_deep(filename:join([ProjDir, "priv"]), Priv),
    
    Deps = filename:join(RootDir, "deps"),
    ok = file:make_dir(Deps),
    copy_deep(filename:join([ProjDir, "deps"]), Deps),
    ?OSCMD("tar cvf "++filename:join(RpmSources, C#config.app++"-"++Version++".tar.gz")
            ++" -C "++RpmSources++" ."),
    ?OSCMD("rm -rf "++RootDir).

copy_folder(Src, Target, Folder, Match) ->
    ok = file:make_dir(filename:join([Target|Folder])),
    [begin
        {ok, _} = file:copy(filename:join([Src|Folder]++[F]),
                            filename:join([Target|Folder]++[F]))
     end
     || F <- filelib:wildcard(Match, filename:join([Src|Folder]))].

copy_deep(ProjDep, TargetDep) ->
    [case D of
        ".git" -> skip;
        "ebin" -> skip;
        ".gitignore" -> skip;       
        D ->
            case filelib:is_dir(filename:join(ProjDep, D)) of
                true ->
                    ok = file:make_dir(filename:join(TargetDep, D)),
                    copy_deep(filename:join(ProjDep, D)
                            , filename:join(TargetDep, D));
                false ->
                    SrcFile = filename:join(ProjDep, D),
                    DstFile = filename:join(TargetDep, D),
                    {ok, #file_info{mode = Mode}} = file:read_file_info(SrcFile),
                    {ok, _} = file:copy(SrcFile, DstFile),
                    ok = file:change_mode(DstFile, Mode)
            end
     end
     || D <- filelib:wildcard("*", ProjDep)].

make_spec() ->
    C = get(config),
    ProjDir = C#config.topDir,
    SpecsFolder = filename:join(C#config.buildPath, "SPECS"),
    SpecFile = filename:join(SpecsFolder, C#config.app++".spec"),
    Version = C#config.version,
    App = C#config.app,
    ?L("Writing Specs to ~s", [SpecFile]),

    {ok, FileH} = file:open(SpecFile, [write, raw]),
    ok = file:write(FileH,
        "Name:           "++App++"\n"
        "Version:        "++Version++"\n"
        "Release:        1%{?dist}\n"
        "Summary:        "++C#config.desc++"\n"
        "\n"
        "Group:          Applications/Communications\n"
        "Vendor:         K2 Informatics, GmbH\n"
        "License:        Proprietary\n"
        "Packager:       Bikram Chatterjee <bikram@bluewin.ch>\n"
        "URL:            http://www.k2informatics.ch\n"
        "\n"
        "BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-"
                                                      "%(%{__id_u} -n)\n"
        "Source:         "++App++"-"++Version++".tar.gz\n"),

    % Description
    ok = file:write(FileH,
        "\n"
        "%description\n"),

    ErlVer = erlang:system_info(version),
    ok = file:write(FileH,
        "\n"
        "%define _topdir "++filename:absname(C#config.buildPath)++"\n"
        "%define _localstatedir /var/opt/"++App++"\n"
        "%define _bindir /opt/"++App++"/bin\n"
        "%define _sbindir /opt/"++App++"/bin\n"
        "%define _sysconfdir /etc\n"
        "%define _installdir /opt/"++App++"\n"
        "%define _reldir /opt/"++App++"/releases\n"
        "%define _erts /opt/"++App++"/erts-"++ErlVer++"\n"
        "%define _libdir /opt/"++App++"/lib\n"
        "%define _etcdir /opt/"++App++"/etc\n"
        "%define _config /opt/"++App++"/config\n"
        "%define _run /var/run/"++App++"\n"
        "%define _pipe /tmp/opt/"++App++"\n"
        "%define _smp_mflags  -j3\n"
        "%define __arch_install_post   /usr/lib/rpm/check-rpaths   /usr/lib/rpm/check-buildroot\n"
        "%define init_script %{_sysconfdir}/init.d/"++App++"\n"
        "%define debug_package %{nil}\n"
        "%global __prelink_undo_cmd %{nil}\n"),

    % Prep
    ok = file:write(FileH,
        "\n"
        "%prep\n"
        "%setup -q -n "++App++"-"++Version++"\n"),

    % Build
    ok = file:write(FileH,
        "\n"
        "%build\n"
        "./rebar compile\n"
        "./rebar generate skip_deps=true\n"),

    % Install
    ok = file:write(FileH,
        "\n"
        "%install\n"
        "%define relpath       %{_builddir}/%{buildsubdir}/rel/"++App++"\n"
        "%define buildroot_etc %{buildroot}%{_etcdir}\n"
        "\n"
        "mkdir -p %{buildroot_etc}\n"
        "mkdir -p %{buildroot}%{_libdir}\n"
        "mkdir -p %{buildroot}%{_reldir}\n"
        "mkdir -p %{buildroot}%{_erts}\n"
        "mkdir -p %{buildroot}%{_config}\n"
        "mkdir -p %{buildroot}%{_run}\n"
        "mkdir -p %{buildroot}%{_pipe}\n"
        "mkdir -p %{buildroot}%{_localstatedir}/log/"++App++"\n"
        "\n"
        "cp -R %{relpath}/etc       %{buildroot}%{_installdir}\n"
        "cp -R %{relpath}/lib       %{buildroot}%{_installdir}\n"
        "cp -R %{relpath}/erts-*    %{buildroot}%{_installdir}\n"
        "cp -R %{relpath}/releases  %{buildroot}%{_installdir}\n"
        "\n"
        "mkdir -p %{buildroot}%{_bindir}\n"
        "if [ -d %{relpath}/bin ]; then \\\n"
        "   find %{relpath}/bin -type f \\\n"
        "        -exec install -p -D -m 0755 {} %{buildroot}%{_bindir}/ \\; "
                                                                      ";fi\n"
        "\n"
        %"mkdir -p %{buildroot}%{_mandir}/man1\n"
        "if [ -d %{_builddir}/%{buildsubdir}/doc/man/man1 ]; then \\\n"
        "   echo -n; fi\n"
        "\n"
        "mkdir -p %{buildroot}%{_localstatedir}/log/"++App++"\n"
        "mkdir -p %{buildroot}%{_sysconfdir}/init.d\n"
        "install -m755 %{buildroot_etc}/init.script  %{buildroot}%{_sysconfdir}"
                                                                "/init.d/"++App++"\n"
        "\n"
        "# Needed to work around check-rpaths which seems to be hardcoded into"
        " recent\n"
        "# RPM releases\n"
        "export QA_RPATHS=3\n"),

    % Clean
    ok = file:write(FileH,
        "\n"
        "%clean\n"
        "rm -rf %{buildroot}\n"),

    % Files
    ok = file:write(FileH,
        "\n"
        "%files\n"
        "%defattr(-,"++App++","++App++")\n"
        "%doc LICENSE\n"
        "%doc README.md\n"
        "%doc RELEASE-"++string:to_upper(App)++".md\n"
        "%{_bindir}\n"
        "%{_erts}/*\n"
        "%{_etcdir}/*\n"
        "%{_libdir}/*\n"
        "%{_reldir}/*\n"
        "%config(noreplace) %{_reldir}/"++Version++"/sys.config\n"
        "%config(noreplace) %{_reldir}/"++Version++"/vm.args\n"
        "%{_config}\n"
        "%{_run}\n"
        "%{_pipe}\n"
        "%{_localstatedir}/log/"++App++"\n"
        "%{_sysconfdir}/init.d/"++App++"\n"
        %"%{_mandir}/man1\n"
        "\n"),

    % Change Log
    ok = file:write(FileH,
        "%changelog\n"),

    % Pre Install
    ok = file:write(FileH,
        "\n"
        "%pre\n"
        "if ! getent group "++App++" >/dev/null 2>&1; then\n"
        "   groupadd -r "++App++"\n"
        "fi\n"
        "\n"
        "if getent passwd "++App++" >/dev/null 2>&1; then\n"
        "   usermod -d %{_localstatedir} "++App++"\n"
        "else\n"
        "   useradd -r -g "++App++" \\\n"
        "           --home %{_localstatedir} \\\n"
        "           --comment \"Dderl user\" \\\n"
        "           "++App++"\n"
        "fi\n"),

    % Pre Un-Install
    ok = file:write(FileH,
        "\n"
        "%preun\n"
        "chkconfig --del "++App++"\n"),

    % Post Install
    ok = file:write(FileH,
        "\n"
        "%post\n"
        "# Fixup perms for SELinux (if it is enabled)\n"
        "selinuxenabled && \\\n"
        "   find %{_localstatedir} -name \"*.so\" \\\n"
        "       -exec chcon -t textrel_shlib_t {} \\;\n"
        "\n"
        "# Softlinks to sys.config and vm.args for easy access\n"
        "if ! [ -L '%{_config}/sys.config' ]; then\n"
        "   ln -s %{_reldir}/"++Version++"/sys.config %{_config}/sys.config\n"
        "fi\n"
        "if ! [ -L '%{_config}/vm.args' ]; then\n"
        "ln -s %{_reldir}/"++Version++"/vm.args %{_config}/vm.args\n"
        "fi\n"
        "\n"
        "# Make sure shell library file is readable\n"
        "chmod 0755 %{_libdir}/env.sh\n"
        "chown -R "++App++":"++App++" %{_installdir}\n"
        "chown -R "++App++":"++App++" %{_localstatedir}\n"
        "chown -R "++App++":"++App++" %{_run}\n"
        "chown -R mpro:mpro %{_pipe}\n"
        "chkconfig --add "++App++"\n"),

    ok = file:close(FileH).
