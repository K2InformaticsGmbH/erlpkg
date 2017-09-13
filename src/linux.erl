-module(linux).
-include("erlpkg.hrl").

-export([build/1]).

build(#{app := Proj, pkgDir := PkgDir} = C0) ->
    RpmBuild = os:find_executable("rpmbuild"),
    Rsync = os:find_executable("rsync"),
    Tar = os:find_executable("tar"),
    C1 = C0#{rpmbuild => RpmBuild, rsync => Rsync, tar => Tar},
    case C1 of
        #{rpmbuild := false} ->
            rebar_api:abort("{~p,~p} rpmbuild not found ", [?MODULE, ?LINE]);
        #{rsync := false} ->
            rebar_api:abort("{~p,~p} rsync not found ", [?MODULE, ?LINE]);
        #{tar := false} ->
            rebar_api:abort("{~p,~p} tar not found ", [?MODULE, ?LINE]);
        _ -> ok
    end,
    ?OSCMD("rm -rf "++PkgDir),
    case catch file:make_dir(PkgDir) of
        ok -> ok;
        Error -> rebar_api:abort("{~p,~p} failed to create ~p : ~p",
                                 [?MODULE, ?LINE, PkgDir, Error])
    end,

    % Build a source tarball
    [begin
        Dir = ?FNJ(PkgDir, D),
        case catch file:make_dir(Dir) of
            ok -> ok;
            Err -> rebar_api:abort("{~p,~p} failed to create ~p : ~p",
                                   [?MODULE, ?LINE, Dir, Err])
        end
     end || D <- ["BUILD", "BUILDROOT", "RPMS", "SOURCES", "SPECS", "SRPMS"]],
    C2 = build_dir_trees(C1),

    % Build Spec
    C3 = make_spec(C2),

    ?I("Building rpm, this may take a while..."),
    C4 = start_time(C3, rpmbuild),
    erlpkg_utils:run_port(RpmBuild, ["-vv", "-ba", ?FNJ(?FNJ(PkgDir, "SPECS"), Proj++".spec")]),
    end_time(C4, rpmbuild).

build_dir_trees(#{app := Proj, rsync := Rsync, relAppDir := Source,
                  pkgDir := PkgDir, version := Version, tar := Tar} = C0) ->
    C1 = start_time(C0, build_dir_trees),
    SrcDir = ?FNJ(Source,"./")++"/",
    DstSources = ?FNJ(PkgDir, "SOURCES"),
    DstSrcDir = ?FNJ(DstSources, Proj++"-"++Version),
    erlpkg_utils:run_port(
        Rsync,
        ["-ar", "--progress", SrcDir, DstSrcDir,
         "--exclude", "bin", "--exclude", "erts-*", "--exclude", "ebin",
         "--exclude", "erloci*/priv", "--exclude", "erlscrypt*/priv"]),
    ?I("source dir tree built in ~s", [DstSrcDir]),
    DstBuildDir = ?FNJ([PkgDir, "BUILD", Proj++"-"++Version]),
    erlpkg_utils:run_port(
        Rsync,
        ["-ar", "--progress", SrcDir, DstBuildDir, "--exclude", "src"]),
    ?I("binary dir tree built in ~s", [DstBuildDir]),
    TarBall = ?FNJ(DstSources, Proj++"-"++Version++".tar.gz"),
    erlpkg_utils:run_port(Tar, ["cvzf", TarBall, "-C", DstSources,
                                Proj++"-"++Version]),
    ?I("~s tarball into ~s", [DstSrcDir, TarBall]),
    ?OSCMD("rm -rf "++DstSrcDir),
    ?I("removed ~s", [DstSrcDir]),
    end_time(C1, build_dir_trees).

make_spec(#{app := App, pkgDir := PkgDir, version := Version,
            desc := Description} = C0) ->
    C1 = start_time(C0, make_spec),
    SpecFile = ?FNJ([PkgDir, "SPECS", App++".spec"]),
    ?I("Writing Specs to ~s", [SpecFile]),

    {ok, FileH} = file:open(SpecFile, [write, raw]),
    ok = file:write(FileH,
        "Name:           "++App++"\n"
        "Version:        "++Version++"\n"
        "Release:        1%{?dist}\n"
        "Summary:        "++Description++"\n"
        "\n"
        "Group:          Applications/Communications\n"
        "Vendor:         K2 Informatics, GmbH\n"
        "License:        Proprietary\n"
        "Packager:       Bikram Chatterjee <chatterjee@bluewin.ch>\n"
        "URL:            http://www.k2informatics.ch\n"
        "\n"
        "BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-"
                                                      "%(%{__id_u} -n)\n"
        "AutoReqProv:    no\n"
        "Source:         "++App++"-"++Version++".tar.gz\n"),

    % Description
    ok = file:write(FileH,
        "\n"
        "%description\n"),

    ErlVer = erlang:system_info(version),
    ok = file:write(FileH,
        "\n"
        "%define _topdir "++filename:absname(PkgDir)++"\n"
        "%define _localstatedir /var/opt/"++App++"\n"
        "%define _bindir /opt/"++App++"/bin\n"
        "%define _sbindir /opt/"++App++"/bin\n"
        "%define _sysconfdir /etc\n"
        "%define _installdir /opt/"++App++"\n"
        "%define _reldir /opt/"++App++"/releases\n"
        "%define _erts /opt/"++App++"/erts-"++ErlVer++"\n"
        "%define _libdir /opt/"++App++"/lib\n"
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
        "%build\n"),

    % Install
    ok = file:write(FileH,
        "\n"
        "%install\n"
        "%define relpath       %{_builddir}/"++App++"-"++Version++"\n"
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
        "if [ -L '%{_config}/sys.config' ]; then\n"
        "   rm '%{_config}/sys.config'\n"
        "fi\n"
        "ln -s %{_reldir}/"++Version++"/sys.config %{_config}/sys.config\n"
        "if [ -L '%{_config}/vm.args' ]; then\n"
        "   rm '%{_config}/vm.args'\n"
        "fi\n"
        "ln -s %{_reldir}/"++Version++"/vm.args %{_config}/vm.args\n"
        "\n"
        "# Make sure shell library file is readable\n"
        "chmod 0755 %{_libdir}/env.sh\n"
        "chown -R "++App++":"++App++" %{_installdir}\n"
        "chown -R "++App++":"++App++" %{_localstatedir}\n"
        "chown -R "++App++":"++App++" %{_run}\n"
        "chown -R mpro:mpro %{_pipe}\n"
        "chkconfig --add "++App++"\n"),

    ok = file:close(FileH),
    end_time(C1, make_spec).

start_time(C, Field) ->
    C#{stats => (maps:get(stats, C, #{}))#{Field => os:timestamp()}}.

end_time(C, Field) ->
    case C of
        #{stats := #{Field := Start} = Stats} ->
            C#{stats => Stats#{Field => timer:now_diff(os:timestamp(), Start)}};
        C ->
            ?W("Stat ~p is not defined", [Field]),
            C
    end.
