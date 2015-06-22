#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -mnesia debug verbose
-include_lib("kernel/include/file.hrl").

-define(H(__F), integer_to_list(erlang:phash2(__F), 16)).

-define(TRACE,  io:format("TRACE ~p~n", [?LINE])).

-record(item, { id
              , type % file | dir | component
              , guid
              , name
              , path
              , file_info
        }).

-define(E(__Fmt,__Args), io:format("[~p] "++__Fmt++"~n", [?LINE | __Args])).
-define(E(__Fmt), ?E(__Fmt,[])).

-define(L(__Fmt,__Args),
        (fun() ->
                 case get(verbose) of
                     V when V == true; V == undefined ->
                         io:format("[~p] "++__Fmt++"~n", [?LINE | __Args]),
                         if V == undefined -> put(verbose, true);
                            true -> ok end;
                     false -> ok
                 end
         end)()).
-define(L(__Fmt), ?L(__Fmt,[])).

-define(OSCMD(__Cmd),
    (fun() ->
        CR = os:cmd(__Cmd),
        CmdResp = re:replace(CR, "[\r\n ]*$", "", [{return, list}]),
        ?L(__Cmd++": ~s", [CmdResp]),
        CmdResp
    end)()
).

-define(FNJ(__Parts), filename:join(__Parts)).

-record(config, {app, desc, version, tmpSrcDir, topDir, rebar, candle, light,
                 msiPath, pkgName, pkgCompany, pkgComment, privFolders, upgradeCode}).

main(main) ->
    io:format(user, "[~p] build_msi with verbose ~p~n", [?LINE, get(verbose)]),
    ScriptPath = filename:absname(escript:script_name()),
    RootPath = case lists:reverse(filename:split(ScriptPath)) of
                   ["build_msi.escript", "windows", "erlpkg", "deps"
                    | RootPathPartsRev] ->
                       filename:join(lists:reverse(RootPathPartsRev));
                   _ ->
                       exit({"owner project path not found",
                              ScriptPath})
               end,
    Rebar = case os:find_executable("rebar") of
        false ->
            case os:find_executable("rebar", RootPath) of
                false -> exit("rebar not found");
                R-> R
            end;
        R -> R
    end,
    Candle = case os:find_executable("candle.exe") of
                 false -> exit("candle.exe not found, wix not installed?");
                 Cndl -> Cndl
             end,
    Light = case os:find_executable("light.exe") of
                 false -> exit("light.exe not found, wix not installed?");
                 L -> L
             end,
    SDir = ?FNJ([RootPath, "src"]),
    AppSrcData = case filelib:is_dir(SDir) of
                 false -> get_app_src_from_rebar_conf(RootPath);
                 true -> get_app_src(SDir)
             end,
    {App, Desc, Version} = app_info_from_app_src(AppSrcData),
    ReleaseTopDir = ?FNJ([RootPath, "rel", "erlpkg_release"]),
    case filelib:is_dir(ReleaseTopDir) of
        false ->
            ok = file:make_dir(ReleaseTopDir),
            ?L("Created ~s", [ReleaseTopDir]);
        _ -> ok
    end,
    MsiPath = ?FNJ([ReleaseTopDir,"msi"]),
    case filelib:is_dir(MsiPath) of
        false ->
            ok = file:make_dir(MsiPath),
            ?L("Created ~s", [MsiPath]);
        _ -> ok
    end,
    AppStr = atom_to_list(App),
    Conf = #config{app = AppStr, desc = Desc, rebar = Rebar, light = Light,
                   topDir = RootPath, version = Version, candle = Candle,
                   tmpSrcDir = ?FNJ([ReleaseTopDir, AppStr++"-"++Version]),
                   msiPath = MsiPath},
    put(config, Conf),

    % Copying application specific files
    [begin
         copy_first_time(F),
         copy_to_msi(F)
     end || F <- ["ServiceSetupDlg.wxs", "banner493x58.jpg",
                  "dialog493x312.jpg", "application.ico", "License.rtf"]],
    copy_first_time("erlpkg.conf"),
    {ok, Config} = file:consult(?FNJ([RootPath,"rel","files","erlpkg.conf"])),
    PkgName = case proplists:get_value(name, Config, '$not_found') of
                  '$not_found' -> "Application Name";
                  PName -> PName
              end,
    PkgCompany = case proplists:get_value(company, Config, '$not_found') of
                  '$not_found' -> "Name of the Company";
                  Comp -> Comp
              end,
    PkgComment = case proplists:get_value(comment, Config, '$not_found') of
                  '$not_found' -> PkgName++" is a registered trademark of "++PkgCompany;
                  PCmnt -> PCmnt
              end,
    PrivFolders = case proplists:get_value(privfolders, Config, '$not_found') of
                  '$not_found' -> "*";
                  PrivDirs -> PrivDirs
              end,
    UpgradeCode = case proplists:get_value(upgradecode, Config, '$not_found') of
                  '$not_found' -> '$no_upgrade_code_defined';
                  UpCode -> UpCode
              end,
    put(config, Conf#config{pkgName = PkgName, pkgCompany = PkgCompany,
                            pkgComment = PkgComment, privFolders = PrivFolders,
                            upgradeCode = UpgradeCode}),
    C = get(config),
    ?L("--------------------------------------------------------------------------------"),
    ?L("packaging ~p (~s) of version ~s", [C#config.app, C#config.desc, C#config.version]),
    ?L("--------------------------------------------------------------------------------"),
    ?L("name        : ~s", [C#config.pkgName]),
    ?L("company     : ~s", [C#config.pkgCompany]),
    ?L("comment     : ~s", [C#config.pkgComment]),
    ?L("priv dirs   : ~p", [C#config.privFolders]),
    ?L("upgrade     : ~p", [C#config.upgradeCode]),
    ?L("app root    : ~s", [C#config.topDir]),
    ?L("tmp src     : ~s", [C#config.tmpSrcDir]),
    ?L("MSI path    : ~s", [C#config.msiPath]),
    ?L("rebar       : ~s", [C#config.rebar]),
    ?L("candle.exe  : ~s", [C#config.candle]),
    ?L("light.exe   : ~s", [C#config.light]),
    ?L("--------------------------------------------------------------------------------"),
    build_msi();
main(Opts) when length(Opts) > 0 ->
    case lists:member("-v", Opts) of
        true -> put(verbose, true);
        false -> put(verbose, false)
    end,
    case lists:member("-sb", Opts) of
        true -> put(skip_build, true);
        false -> put(skip_build, false)
    end,
    case lists:member("-sg", Opts) of
        true -> put(skip_generate, true);
        false -> put(skip_generate, false)
    end,
    main(main);
main([]) ->
    put(verbose, false),
    put(skip_build, false),
    put(skip_generate, false),
    main(main).

copy_first_time(File) ->
    C = get(config),
    case filelib:is_file(?FNJ([C#config.topDir,"rel","files",File])) of
        true ->
            ?L("override for ~s found in rel/files", [File]);
        false ->
            Src = ?FNJ([C#config.topDir,"deps","erlpkg","windows",File]),
            Dst = ?FNJ([C#config.topDir,"rel","files",File]),
            ?L("checking ~s", [Dst]),
            case filelib:ensure_dir(Dst) of
                ok -> ok;
                {error, Error1} ->
                    exit({"failed to create path for "++Dst, Error1})
            end,
            case file:copy(Src, Dst) of
                {error, Error} ->
                    exit({"failed to copy "++File, Error});
                {ok, _BytesCopied} ->
                   ?L("copied ~s to rel/files", [File])
            end
    end.

copy_to_msi(File) ->
    C = get(config),
    case file:copy(?FNJ([C#config.topDir,"rel","files",File]),
                   ?FNJ([C#config.msiPath,File])) of
        {error, Error} ->
            exit({"failed to copy "++File, Error});
        {ok, _BytesCopied} ->
            ?L("copied ~s to ~s", [File, C#config.msiPath])
    end.

get_app_src(SDir) ->
    case filelib:wildcard("*.app.src", SDir) of
        [AppSrcFile] ->
            case catch file:consult(?FNJ([SDir, AppSrcFile])) of
                {'EXIT', Error} -> exit({"bad file", AppSrcFile, Error});
                {ok, AppSrcFileData} -> AppSrcFileData
            end;
        Else -> exit({"unable to proceed with following app.src info", Else})
    end.

get_app_src_from_rebar_conf(RootPath) ->
    case catch file:consult(?FNJ([RootPath, "rebar.config"])) of
        {'EXIT', Error} ->
            exit({"rebar.config not found", Error});
        {ok, RebarConf} ->
            case proplists:get_value(sub_dirs, RebarConf) of
                undefined ->
                    exit("sub_dirs not defined in rebar.config");
                SubDirs ->
                    case lists:foldl(
                           fun(Dir, undefined) ->
                                   case catch get_app_src(
                                                ?FNJ([RootPath,Dir,"src"])) of
                                       {'EXIT', _} -> undefined;
                                       AppSrc -> AppSrc
                                   end;
                              (_Dir, AppSrc) -> AppSrc
                           end, undefined, SubDirs) of
                        undefined ->
                            exit({"src dir not found", SubDirs});
                        AppSrcFileData -> AppSrcFileData
                    end
            end
    end.

app_info_from_app_src(AppSrcData) ->
    case lists:keyfind(application, 1, AppSrcData) of
        false -> exit({"malformed", AppSrcData});
        {application, AppName, AppConfig} ->
            Desc = case proplists:get_value(description, AppConfig) of
                       undefined -> "";
                       D -> D
                   end,
            Version = case proplists:get_value(vsn, AppConfig) of
                       undefined -> exit("version not defined");
                       V -> V
                   end,
            {AppName, Desc, Version}
    end.

build_msi() ->
    C = get(config),
    case get(skip_build) of
        false ->
            build_sources(),
            ?L("source tree built");
        _ ->
            ?L("skipped source tree build")
    end,
    case get(skip_generate) of
        false ->
            rebar_generate(),
            ?L("OTP release prepared");
        _ ->
            ?L("skipped rebar generate")
    end,
    case file:copy(?FNJ([C#config.topDir,"deps","erlpkg","windows",
                         "service.escript"]),
                   ?FNJ([C#config.tmpSrcDir,"rel",C#config.app,"bin",
                         C#config.app++".escript"])) of
        {error, Error} ->
            exit({"failed to copy service.escript", Error});
        _ -> ok
    end,
    {ok, _} = dets:open_file(C#config.app++"ids",
                             [{ram_file, true},
                              {file, C#config.app++"ids.dets"}, {keypos, 2}]),
    create_wxs(),
    candle_light(),
    ok = dets:close(C#config.app++"ids").
    
uuid() ->
    string:to_upper(re:replace(os:cmd("uuidgen.exe"), "\r\n", "",
                               [{return, list}])).

run_port(Cmd, Args) ->
    log_cmd(Cmd,
            erlang:open_port({spawn_executable, Cmd},
                             [{line, 128},{args, Args}, exit_status,
                              stderr_to_stdout, {parallelism, true}])).
run_port(Cmd, Args, Cwd) ->
    log_cmd(Cmd,
            erlang:open_port({spawn_executable, Cmd},
                             [{cd, Cwd},{line, 128},{args, Args},
                              exit_status,stderr_to_stdout,
                              {parallelism, true}])).

-define(NL(__Fmt,__Args), io:format(__Fmt, __Args)).
-define(NL(__Fmt), ?NL(__Fmt,[])).
log_cmd(Cmd, Port) when is_port(Port) ->
    receive
        {'EXIT',Port,Reason} -> ?E("~s terminated for ~p", [Cmd, Reason]);
        {Port,closed} -> ?E("~s terminated", [Cmd]);
        {Port,{exit_status,Status}} ->
            ?E("~s exit with status ~p", [Cmd, Status]),
            catch erlang:port_close(Port);
        {Port,{data,{F,Line}}} ->
            ?NL("~s" ++ if F =:= eol -> "~n"; true -> "" end, [Line]),
            log_cmd(Cmd, Port);
        {Port,{data,Data}} ->
            ?NL("~p", [Data]),
            log_cmd(Cmd, Port)
    end.

build_sources() ->
    C = get(config),
    ?L("Source ~s", [C#config.topDir]),
    ?L("Build Source in ~s", [C#config.tmpSrcDir]),
    ?OSCMD("rm -rf "++C#config.tmpSrcDir),
    ok = file:make_dir(C#config.tmpSrcDir),
    [begin
        {ok, _} = file:copy(filename:join(C#config.topDir,F),
                        filename:join(C#config.tmpSrcDir,F))
    end || F <- ["rebar.config", "LICENSE", "README.md",
                 "RELEASE-"++string:to_upper(C#config.app)++".md"]],
    ?OSCMD("cp -L \""++C#config.rebar++"\" \""++C#config.tmpSrcDir++"\""),
    ?OSCMD("cp -L \""++filename:rootname(C#config.rebar)++"\" \""
           ++C#config.tmpSrcDir++"\""),
    ok = file:write_file(filename:join(C#config.tmpSrcDir,"rebar.bat"),
                         <<"rebar.cmd %*\n">>),
    PathPrfxLen = length(C#config.topDir)+2,
    copy_folder(PathPrfxLen, C#config.topDir, C#config.tmpSrcDir, ["include"], "*.*"),
    copy_folder(PathPrfxLen, C#config.topDir, C#config.tmpSrcDir, ["src"], "*.*"),
    copy_folder(PathPrfxLen, C#config.topDir, C#config.tmpSrcDir, ["docs"], "*.*"),
    copy_folder(PathPrfxLen, C#config.topDir, C#config.tmpSrcDir, ["rel"], "*.*"),
    copy_folder(PathPrfxLen, C#config.topDir, C#config.tmpSrcDir, ["rel", "files"], "*"),

    Priv = filename:join(C#config.tmpSrcDir, "priv"),
    ok = file:make_dir(Priv),
    case C#config.privFolders of
        "*" ->
            copy_deep(PathPrfxLen, filename:join([C#config.topDir, "priv"]), Priv);
        Folders ->
            [begin
                 TargetDir = ?FNJ([Priv, Folder]),                 
                 ok = file:make_dir(TargetDir),
                 copy_deep(PathPrfxLen, ?FNJ([C#config.topDir, "priv", Folder]),
                           TargetDir)
             end || Folder <- Folders]
    end,

    Deps = filename:join(C#config.tmpSrcDir, "deps"),
    ok = file:make_dir(Deps),
    copy_deep(PathPrfxLen, filename:join([C#config.topDir, "deps"]), Deps).

copy_folder(PathPrefixLen, Src, Target, Folders, Match) ->
    ok = file:make_dir(filename:join([Target|Folders])),
    SrcFolder = filename:join([Src|Folders]),    
    ?L("cp ~s", [string:substr(SrcFolder, PathPrefixLen)]),
    [begin
         Source = filename:join(SrcFolder,F),
         IsFile = (filelib:is_dir(Source) == false andalso
                   filename:extension(Source) /= ".swp"),
         if IsFile ->
                {ok, _} = file:copy(Source,
                                    filename:join([Target|Folders]++[F]));
                true -> ok
         end
     end || F <- filelib:wildcard(Match, SrcFolder)].

copy_deep(PathPrefixLen, ProjDep, TargetDep) ->
    [case D of
        ".git" -> skip;
        "ebin" -> skip;
        ".gitignore" -> skip;       
        D ->
             Src = filename:join(ProjDep, D),
             Dst = filename:join(TargetDep, D),
             case filelib:is_dir(Src) of
                 true ->
                     ?L("cp ~s", [string:substr(Src, PathPrefixLen)]),
                     ok = file:make_dir(Dst),
                     copy_deep(PathPrefixLen, Src, Dst);
                 false ->
                     {ok, #file_info{mode = Mode}} = file:read_file_info(Src),
                     {ok, _} = file:copy(Src, Dst),
                     ok = file:change_mode(Dst, Mode)
             end
     end
     || D <- filelib:wildcard("*", ProjDep)].

rebar_generate() ->
    C = get(config),
    file:delete(C#config.app++"ids.dets"),
    {ok, CurDir} = file:get_cwd(),
    ?L("Entering ~s from ~s", [C#config.tmpSrcDir, CurDir]),
    ok = file:set_cwd(C#config.tmpSrcDir),
    ?L("Clean Compile and generate..."),
    Verbose = get(verbose), 
    run_port("rebar.bat", if Verbose -> ["-v"]; true -> [] end ++ ["clean"], C#config.tmpSrcDir),
    run_port("rebar.bat", if Verbose -> ["-v"]; true -> [] end ++ ["compile"], C#config.tmpSrcDir),
    run_port("rebar.bat", if Verbose -> ["-v"]; true -> [] end ++ ["generate", "skip_deps=true"], C#config.tmpSrcDir),
    ?L("Leaving ~s to ~s", [C#config.tmpSrcDir, CurDir]),
    ok = file:set_cwd(CurDir).

create_wxs() ->
    C = get(config),
    Proj = C#config.app,
    Version = C#config.version,
    Root = C#config.tmpSrcDir,
    Tab = Proj++"ids",
    {ok, FileH} = file:open(
                    filename:join([C#config.msiPath,
                                   lists:flatten([Proj,"-",Version,".wxs"])]),
                    [write, raw]),
    {ok, PRODUCT_GUID} = get_id(Tab, undefined, 'PRODUCT_GUID', undefined),
    {ok, UPGRADE_GUID} = case C#config.upgradeCode of
                             '$no_upgrade_code_defined' ->
                                 get_id(Tab, undefined, 'UPGRADE_GUID', undefined);
                             UpCode -> {ok, UpCode}
                         end,
    {ok, ID} = get_id(Tab, undefined, C#config.pkgCompany, undefined),
    ok = file:write(FileH,
        "<?xml version='1.0' encoding='windows-1252'?>\n"
        "<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'"
        "     xmlns:util='http://schemas.microsoft.com/wix/UtilExtension'>\n\n"

        "<Product Name='"++C#config.pkgName++"'\n"
        "         Id='"++PRODUCT_GUID++"'\n"
        "         UpgradeCode='"++UPGRADE_GUID++"'\n"
        "         Language='1033' Codepage='1252' Version='"++Version++"'\n"
        "         Manufacturer='"++C#config.pkgCompany++"'>\n\n"

        "   <Package Id='*'\n"
        "            Keywords='Installer'\n"
        "            Description=\""++C#config.pkgCompany++"\"\n"
        "            Comments='"++C#config.pkgComment++"'\n"
        "            Manufacturer='"++C#config.pkgCompany++"'\n"
        "            InstallerVersion='200' Languages='1033'\n"
        "            Compressed='yes'\n"
        "            InstallScope='perMachine'\n"
        "            InstallPrivileges='elevated'\n"
        "            SummaryCodepage='1252' />\n\n"

        "   <MajorUpgrade DowngradeErrorMessage='A later version of [ProductName]"
                             " is already installed. Setup will now exit.' />\n\n"

        "   <Media Id='1' Cabinet='"++Proj++".cab' EmbedCab='yes'\n"
        "          DiskPrompt='CD-ROM #1'/>\n"
        "   <Property Id='DiskPrompt'\n"
        "             Value=\""++C#config.pkgCompany++" "++Proj++" Installation [1]\"/>\n\n"),

    ?L("wxs header sections created"),

    ok = file:write(FileH,
        "   <Directory Id='TARGETDIR' Name='SourceDir'>\n"),

    % AppData PATH
    {CoDatId, CoDatGuId} = get_id(Tab, component, 'COMPANYDAT_GUID', undefined),
    {AppDatId, AppDatGuId} = get_id(Tab, component, 'PRODUCTDAT_GUID', undefined),
    ok = file:write(FileH,
        "     <Directory Id='CommonAppDataFolder' Name='CommonAppData'>\n"
        "       <Directory Id='COMPANYDAT' Name='"++C#config.pkgCompany++"'>\n"
        "         <Component Id='"++CoDatId++"' Guid='"++CoDatGuId++"'>\n"
        "           <CreateFolder Directory='COMPANYDAT'>\n"
        "             <Permission User='Everyone' GenericAll='yes' />\n"
        "           </CreateFolder>\n"
        "         </Component>\n"
        "         <Directory Id='PRODUCTDAT' Name='"++C#config.pkgName++"'>\n"
        "           <Component Id='"++AppDatId++"' Guid='"++AppDatGuId++"'>\n"
        "             <CreateFolder Directory='PRODUCTDAT'>\n"
        "               <Permission User='Everyone' GenericAll='yes' />\n"
        "             </CreateFolder>\n"
        "           </Component>\n"
        "         </Directory> <!-- PRODUCT -->\n"
        "       </Directory> <!-- COMPANY -->\n"
        "     </Directory> <!-- CommonAppDataFolder -->\n\n"),

    % ProgramFiles PATH
    ok = file:write(FileH,
        "     <Directory Id='ProgramFilesFolder' Name='PFiles'>\n"
        "       <Directory Id='"++ID++"' Name='"++C#config.pkgCompany++"'>\n"
        "         <Directory Id='INSTALLDIR' Name='"++C#config.pkgName++"'>\n"),

    walk_release(Proj, Tab, FileH, Root),
    ?L("finished walking OTP release"),
    
    ok = file:write(FileH,
        "         </Directory> <!-- PRODUCT -->\n"
        "       </Directory> <!-- COMPANY -->\n"
        "     </Directory> <!-- ProgramFilesFolder -->\n"),

    ?L("finished ProgramFilesFolder section"),

    % Property references
    [BootDir] = dets:select(Tab, [{#item{type=dir, name=Version, _='_'}, [],
                                   ['$_']}]),
    [EscriptExe] = dets:select(Tab, [{#item{type=component, name="escript.exe",
                                            _='_'}, [], ['$_']}]),
    [EscriptExeFile] = dets:select(Tab, [{#item{type=file, name="escript.exe",
                                            guid=undefined, _='_'}, [], ['$_']}]),
    [EditConfEs] = dets:select(Tab, [{#item{type=component,
                                             name="editconfs.escript", _='_'},
                                       [], ['$_']}]),
    [SrvcCtrlEs] = dets:select(Tab, [{#item{type=component,
                                            name=Proj++".escript", _='_'},
                                      [], ['$_']}]),
    [SrvcCtrlEsFile] = dets:select(Tab, [{#item{type=file, name=Proj++".escript",
                                                guid=undefined, _='_'}, [], ['$_']}]),

    {ProgFolderId, ProgFolderGuId} = get_id(Tab, component, 'PROGSMENUFOLDER_GUID', undefined),
    {DsktpShortId, DsktpShortGuId} = get_id(Tab, component, 'DESKTOPSHORTCUT_GUID', undefined),

    ok = file:write(FileH,
        "     <Directory Id='ProgramMenuFolder' Name='Programs'>\n"
        "        <Directory Id='ApplicationProgramMenuFolder'\n"
        "                   Name='"++C#config.pkgName++"' />\n"
        "     </Directory> <!-- ProgramMenuFolder -->\n\n"),

    ?L("finished ProgramMenuFolder section"),

    ok = file:write(FileH,
        "     <Directory Id='DesktopFolder' Name='Desktop'>\n"
        "       <Directory Id='ApplicationDesktopFolder' Name='"++C#config.pkgName++"'/>\n"
        "     </Directory> <!-- DesktopFolder -->\n\n"),

    ?L("finished DesktopFolder section"),

    ok = file:write(FileH,
        "   </Directory> <!-- TARGETDIR -->\n\n"),

    ?L("finished TARGETDIR section"),

    build_features(Proj, Version, FileH),

    ?L("feature sections created"),

    ok = file:write(FileH,
        "   <WixVariable Id='WixUILicenseRtf' Value='License.rtf' />\n"
        "   <WixVariable Id='WixUIBannerBmp' Value='banner493x58.jpg' />\n"
        "   <WixVariable Id='WixUIDialogBmp'"
                       " Value='dialog493x312.jpg' />\n\n"),

    ?L("added banner and dialog images and license"),

    ok = file:write(FileH,
        "   <UIRef Id='WixUI_Mondo' />\n"
        "   <UIRef Id='WixUI_ErrorProgressText' />\n\n"),

    % External Dialog Chaining
    ok = file:write(FileH,
        "   <UI Id='CustWixUI_Mondo'>\n"
        "       <UIRef Id='WixUI_Mondo' />\n"
        "       <UIRef Id='WixUI_ErrorProgressText' />\n\n"

        "       <DialogRef Id='ServiceSetupDlg' />\n"

        "       <Publish Dialog='CustomizeDlg' Control='Next'\n"
        "                Event='NewDialog' Value='ServiceSetupDlg'\n"
        "                Order='3'>LicenseAccepted = 1</Publish>\n"
        "       <Publish Dialog='VerifyReadyDlg' Control='Back'\n"
        "                Event='NewDialog' Value='ServiceSetupDlg'>\n"
        "           1</Publish>\n"
        "   </UI>\n\n"),

    ?L("added custom setup dialog"),

    [VmArgsFile] = dets:select(Tab, [{#item{type=file
                                             , name="vm.args", _='_'}
                                       , [], ['$_']}]),
    [SysConfigFile] = dets:select(Tab, [{#item{type=file
                                             , name="sys.config", _='_'}
                                       , [], ['$_']}]),
    {ok, VmArgsBin} = file:read_file(filename:join(VmArgsFile#item.path, "vm.args")),
    {ok, SysConfigBin} = file:read_file(filename:join(SysConfigFile#item.path, "sys.config")),
    {match, [Node]} = re:run(VmArgsBin
                             , ".*-name (.*)[\r\n]"
                             , [{capture, [1], list}, ungreedy, dotall]),
    {match, [Cookie]} = re:run(VmArgsBin
                               , ".*-setcookie (.*)[\r\n]"
                               , [{capture, [1], list}, ungreedy, dotall]),
    {match, [DDErlIntf]} = re:run(SysConfigBin
                                  , ".*\{interface,[ ]*\"(.*)\"[ ]*}"
                                  , [{capture, [1], list}, ungreedy, dotall]),
    {match, [DDErlPort]} = re:run(SysConfigBin
                                  , ".*\{port,[ ]*([0-9]*)[ ]*}"
                                  , [{capture, [1], list}, ungreedy, dotall]),
    {match, [ImemNodeType]} = re:run(SysConfigBin
                                       , ".*\{mnesia_node_type,[ ]*(disc|ram)[ ]*}"
                                       , [{capture, [1], list}, ungreedy, dotall]),
    {match, [ImemSchemaName]} = re:run(SysConfigBin
                                       , ".*\{mnesia_schema_name,[ ]*'(.*)'[ ]*}"
                                       , [{capture, [1], list}, ungreedy, dotall]),
    {match, [ImemClustMgrs]} = re:run(SysConfigBin
                                      , ".*\{erl_cluster_mgrs,[ ]*(\\[.*)[ ]*}"
                                      , [{capture, [1], list}, ungreedy, dotall]),
    {match, [ImemIntf]} = re:run(SysConfigBin
                                 , ".*\{tcp_ip,[ ]*\"(.*)\"[ ]*}"
                                 , [{capture, [1], list}, ungreedy, dotall]),
    {match, [ImemPort]} = re:run(SysConfigBin
                                 , ".*\{tcp_port,[ ]*([0-9]*)[ ]*}"
                                 , [{capture, [1], list}, ungreedy, dotall]),

    ok = file:write(FileH,
        "   <Property Id='NODENAME'>"++Node++"</Property>\n"
        "   <Property Id='NODECOOKIE'>"++Cookie++"</Property>\n"
        "   <Property Id='WEBSRVINTF'>"++DDErlIntf++":"++DDErlPort++"</Property>\n"
        "   <Property Id='DBNODETYPE'>"++ImemNodeType++"</Property>\n"
        "   <Property Id='DBNODETYPE_DISC'>disc</Property>\n"
        "   <Property Id='DBNODETYPE_RAM'>ram</Property>\n"
        "   <Property Id='DBNODESCHEMANAME'>"++ImemSchemaName++"</Property>\n"
        "   <Property Id='DBCLUSTERMGRS'><![CDATA["++ImemClustMgrs++"]]></Property>\n"
        "   <Property Id='DBINTF'>"++ImemIntf++":"++ImemPort++"</Property>\n\n"),

    % Read real installation folder from registry if exists
    ok = file:write(FileH,
        "   <Property Id='EXISTINGINSTALLDIR' Secure='yes'>\n"
        "       <RegistrySearch Id='Locate_EXISTINGINSTALLDIR' Root='HKCU'\n"
        "                       Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                       Name='InstallPath' Type='directory' />\n"
        "   </Property>\n\n"),

    ?L("added properties connecting to custom setup dialog"),

    %% Service customization
    EscriptExePath = filename:split(EscriptExe#item.path),
    EditConfEsPath = filename:split(EditConfEs#item.path),
    SrvcCtrlEsPath = filename:split(SrvcCtrlEs#item.path),
    ExecCommand = "\"[INSTALLDIR]"
                  ++ string:join(
                       lists:sublist(EscriptExePath
                                     , length(EscriptExePath)-1, 2)
                       ++ ["escript.exe"]
                       , "\\")
                  ++ "\" \"[INSTALLDIR]"
                  ++ string:join(
                       lists:sublist(EditConfEsPath
                                     , length(EditConfEsPath), 1)
                       ++ ["editconfs.escript"]
                       , "\\")
                  ++ "\"",

    SrvcCommand = "\"[INSTALLDIR]"
                  ++ string:join(
                       lists:sublist(EscriptExePath
                                     , length(EscriptExePath)-1, 2)
                       ++ ["escript.exe"]
                       , "\\")
                  ++ "\" \"[INSTALLDIR]"
                  ++ string:join(
                       lists:sublist(SrvcCtrlEsPath
                                     , length(SrvcCtrlEsPath), 1)
                       ++ [Proj++".escript"]
                       , "\\")
                  ++ "\"",

    InsldSrvcCmd = "\"[EXISTINGINSTALLDIR]"
                  ++ string:join(
                       lists:sublist(EscriptExePath
                                     , length(EscriptExePath)-1, 2)
                       ++ ["escript.exe"]
                       , "\\")
                  ++ "\" \"[EXISTINGINSTALLDIR]"
                  ++ string:join(
                       lists:sublist(SrvcCtrlEsPath
                                     , length(SrvcCtrlEsPath), 1)
                       ++ [Proj++".escript"]
                       , "\\")
                  ++ "\"",

    ?L("service control ~s @ ~s", [SrvcCommand, BootDir#item.path]),

    %% Service Installation

    % Custom actions service configure
    %  must run after InstallFiles step is 'comitted'
    %  and before InstallService action it must not
    %  impersonate to retain file modification priviledges
    ok = file:write(FileH,
        "   <CustomAction Id='ConfigService' Directory='"++BootDir#item.id++"'\n"
        "                 ExeCommand='"++ExecCommand++" \"[NODENAME]\" "
                                      "\"[NODECOOKIE]\" \"[WEBSRVINTF]\" "
                                      "\"[DBNODETYPE]\" \"[DBNODESCHEMANAME]\" "
                                      "\"[DBCLUSTERMGRS]\" \"[DBINTF]\" "
                                      "\"["++BootDir#item.id++"]\\\" "
                                      "\"[PRODUCTDAT]\\\"'\n"
        "                 Execute='commit' Impersonate='no' />\n\n"),

    ?L("added service configuration custom action"),

    % Custom actions service install and start
    %  must run after InstallFiles step is 'comitted'
    ok = file:write(FileH,
        "   <CustomAction Id='InstallService' Directory='"++BootDir#item.id++"'\n"
        "                 ExeCommand='"++SrvcCommand++" install \""++C#config.pkgName++"\""
                          " \""++C#config.desc++"\"'\n"
        "                 Execute='commit' Impersonate='no' />\n"
        "   <CustomAction Id='StartService' Directory='"++BootDir#item.id++"'\n"
        "                 ExeCommand='"++SrvcCommand++" start \""++C#config.pkgName++"\"'\n"
        "                 Execute='commit' Impersonate='no' />\n"
    % Custom actions service stop and uninstall
    %  must run immediately and before InstallValidate step to ensure that
    %  installed files are not removed and service is stopped before
    %  uninstalling process detecets and warns
    %  Execute='deferred' is MUST to enforce immediate elivated execution
        "   <CustomAction Id='UnInstallService' Directory='"++BootDir#item.id++"'\n"
        "                 ExeCommand='"++InsldSrvcCmd++" uninstall \""++C#config.pkgName++"\"'\n"
        "                 Execute='deferred' Impersonate='no' />\n"
        "   <CustomAction Id='StopService' Directory='"++BootDir#item.id++"'\n"
        "                 ExeCommand='"++InsldSrvcCmd++" stop \""++C#config.pkgName++"\"'\n"
        "                 Execute='deferred' Impersonate='no' />\n\n"),

    ?L("added service control custom actions"),

    % Sequence of custom action is important to ensure
    %  service is installed before started and stopped
    %  before uninstalling
    % Also ComponentId = # is used to identify the service
    %  controlling script is executed in correct execution path
    %  2 - Uninstalling
    %  3 - Installing
    %  Ref http://wix.tramontana.co.hu/tutorial/com-expression-syntax-miscellanea/expression-syntax
    ok = file:write(FileH,
        "   <InstallExecuteSequence>\n"
        "       <Custom Action='StopService' After='InstallInitialize'><![CDATA["
                    "($"++EscriptExe#item.id++"=2) AND "
                    "($"++SrvcCtrlEs#item.id++"=2)]]></Custom>\n"
        "       <Custom Action='UnInstallService' After='StopService'><![CDATA["
                    "($"++EscriptExe#item.id++"=2) AND "
                    "($"++SrvcCtrlEs#item.id++"=2)]]></Custom>\n"
        "       <Custom Action='ConfigService' After='InstallFiles'><![CDATA["
                    "($"++EscriptExe#item.id++"=3) AND "
                    "($"++EditConfEs#item.id++"=3)]]></Custom>\n"
        "       <Custom Action='InstallService' After='ConfigService'><![CDATA["
                    "($"++EscriptExe#item.id++"=3) AND "
                    "($"++SrvcCtrlEs#item.id++"=3)]]></Custom>\n"
        "       <Custom Action='StartService' After='InstallService'><![CDATA["
                    "($"++EscriptExe#item.id++"=3) AND "
                    "($"++SrvcCtrlEs#item.id++"=3)]]></Custom>\n"
        "   </InstallExecuteSequence>\n\n"),

    ?L("added service start/stop sequence for install/uninstall"),

    ok = file:write(FileH,
        "   <DirectoryRef Id='ApplicationProgramMenuFolder'>\n"
        "       <Component Id='"++ProgFolderId++"' Guid='"++ProgFolderGuId++"'>\n"
        "           <Shortcut Id='programattach'\n"
        "                     Name='"++C#config.pkgName++" Attach'\n"
        "                     Target='[#"++EscriptExeFile#item.id++"]'\n"
        "                     Arguments='\"[#"++SrvcCtrlEsFile#item.id++"]\" attach'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='application.ico' IconIndex='0' />\n"
        "           <Shortcut Id='programgui'\n"
        "                     Name='"++C#config.pkgName++" GUI'\n"
        "                     Target='[#"++EscriptExeFile#item.id++"]'\n"
        "                     Arguments='\"[#"++SrvcCtrlEsFile#item.id++"]\" console'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='application.ico' IconIndex='0' />\n"
        "           <RemoveFolder Id='ApplicationProgramMenuFolder' On='uninstall'/>\n"
        "           <RegistryValue Root='HKCU'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='programmenu' Type='string'\n"
        "                          Value='"++PRODUCT_GUID++"' KeyPath='yes'/>\n"
        % Remember real installation path in registry
        "           <RegistryValue Root='HKCU'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='InstallPath' Type='string'\n"
        "                          Value='[INSTALLDIR]' KeyPath='no'/>\n"
        % Recursively remove application from path
        "           <util:RemoveFolderEx On='uninstall' Property='EXISTINGINSTALLDIR' />\n"
        "       </Component>\n"
        "   </DirectoryRef>\n\n"),

    ?L("added short cuts to ApplicationProgramMenuFolder"),

    ok = file:write(FileH,
        "   <DirectoryRef Id='ApplicationDesktopFolder'>\n"
        "       <Component Id='"++DsktpShortId++"' Guid='"++DsktpShortGuId++"'>\n"
        "           <Shortcut Id='desktopattach'\n"
        "                     Name='"++C#config.pkgName++" Attach'\n"
        "                     Target='[#"++EscriptExeFile#item.id++"]'\n"
        "                     Arguments='\"[#"++SrvcCtrlEsFile#item.id++"]\" attach'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='application.ico' IconIndex='0' />\n"
        "           <Shortcut Id='desktopgui'\n"
        "                     Name='"++C#config.pkgName++" GUI'\n"
        "                     Target='[#"++EscriptExeFile#item.id++"]'\n"
        "                     Arguments='\"[#"++SrvcCtrlEsFile#item.id++"]\" console'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='application.ico' IconIndex='0' />\n"
        "           <RemoveFolder Id='ApplicationDesktopFolder' On='uninstall'/>\n"
        "           <RegistryValue Root='HKCU'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='desktop' Type='integer'\n"
        "                          Value='1' KeyPath='yes'/>\n"
        "       </Component>\n"
        "   </DirectoryRef>\n"),

    ?L("added short cuts to ApplicationDesktopFolder"),

    ok = file:write(FileH,
        "   <Icon Id='application.ico' SourceFile='application.ico' />\n"),

    ok = file:write(FileH,
        "   <Property Id='ARPPRODUCTICON' Value='application.ico' />"),
    
    ok = file:write(FileH,
        "</Product>\n"
        "</Wix>"),

    ?L("finised building wxs"),

    ok = file:close(FileH).

candle_light() ->
    Verbose = get(verbose),
    C = get(config),
    {ok, CurDir} = file:get_cwd(),
    ok = file:set_cwd(C#config.msiPath),
    Wxses = filelib:wildcard("*.wxs"),
    ?L("candle with ~p", [Wxses]),
    run_port(C#config.candle, if Verbose -> ["-v"]; true -> [] end
             ++ ["-ext", "WixUtilExtension" | Wxses]),
    WixObjs = filelib:wildcard("*.wixobj"),
    MsiFile = generate_msi_name(),
    ?L("light ~s with ~p", [MsiFile, WixObjs]),
    run_port(C#config.light, if Verbose -> ["-v"]; true -> [] end
             ++ ["-ext", "WixUtilExtension",
                 "-ext", "WixUIExtension",
                 "-out", MsiFile | WixObjs]),
    ok = file:set_cwd(CurDir).

get_filepath(Dir, F) ->
    FilePathNoRel =
        lists:foldl(
          fun
              ("..", Acc) -> Acc;
              (P,Acc) -> Acc ++ [P]
          end,
          [], filename:split(Dir)),
    filename:join([".." | FilePathNoRel]++[F]).

generate_msi_name() ->
    C = get(config),
    {{Y,M,D},{H,Mn,S}} = calendar:local_time(),
    MsiDate = io_lib:format("~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B",
                            [Y,M,D,H,Mn,S]),
    lists:flatten([C#config.app,"-",C#config.version,"_",MsiDate,".msi"]).

walk_release(Proj, Tab, FileH, Root) ->
    C = get(config),
    ReleaseRoot = filename:join([Root,"rel",Proj]),
    case filelib:is_dir(ReleaseRoot) of
        true ->
            walk_release(length(C#config.tmpSrcDir)+2, Tab, FileH,
                         filelib:wildcard("*", ReleaseRoot), ReleaseRoot, 12);
        false -> ?L("~p is not a directory", [ReleaseRoot])
    end.

walk_release(_PathPrefixLen, _Tab, _FileH, [], _Dir, _N) -> ok;
walk_release(PathPrefixLen, Tab, FileH, [F|Files], Dir, N) ->
    case filelib:is_dir(filename:join([Dir,F])) of
        true ->
            NewDirLevel = filename:join([Dir,F]),
            FilesAtThisLevel = filelib:wildcard("*", NewDirLevel),
            {ok, DirId} = get_id(Tab, dir, F, Dir),
            ok = file:write(FileH, lists:duplicate(N,32)++
                            "<Directory Id='"++DirId++
                                            "' Name='"++F++"'>\n"),
            walk_release(PathPrefixLen, Tab, FileH, FilesAtThisLevel, NewDirLevel, N+3),
            ok = file:write(FileH, lists:duplicate(N,32)++"</Directory>\n"),
            ?L("~s/", [string:substr(NewDirLevel, PathPrefixLen)]);
        false ->
            FilePath = get_filepath(Dir, F),
            {Id, GuID} = get_id(Tab, component, F, Dir),
            {ok, FileId} = get_id(Tab, file, F, Dir),
            ok = file:write(FileH, lists:duplicate(N+3,32)++
                "<Component Id='"++Id++"' Guid='"++GuID++"'>\n"
                ++lists:duplicate(N+3,32)++
                "   <File Id='"++FileId++"' Name='"++F++
                                "' DiskId='1' Source='"++FilePath++"'"
                " KeyPath='yes' />\n"++lists:duplicate(N+3,32)++
                "</Component>\n")
    end,
    walk_release(PathPrefixLen, Tab, FileH, Files, Dir, N).

build_features(Proj, Version, FileH) ->
    Tab = Proj++"ids",
    ok = file:write(FileH,
        "   <Feature Id='Complete' Title='"++Proj++"-"++Version++"'"
                    " Description='The complete package.'"
                    " Level='1' ConfigurableDirectory='INSTALLDIR'>\n"),
    ok = file:write(FileH,
        "      <Feature Id='MainProgram' Title='"++Proj++"-"++Version
                                                        ++" service'"
                    " Description='The service.' Level='1'>\n"),
    dets:sync(Tab),
    dets:traverse(Tab,
                  fun(#item{type=component, id = Id}) ->
                          ok = file:write(FileH
                                          , "         <ComponentRef Id='"
                                          ++Id++"' />\n"),
                          continue;
                     (_) -> continue
                  end),
    ok = file:write(FileH, "      </Feature>\n\n"),
    ok = file:write(FileH, "   </Feature>\n\n").

get_id(Tab, undefined, Field, undefined) when is_list(Field) ->
    Id = "id_"++?H(Field),
    case dets:lookup(Tab, Id) of
        [] ->
            Item = #item{id = Id, name = Field},
            ok = dets:insert(Tab, Item),
            {ok, Item#item.id};
        [#item{} = Item] ->
            {ok, Item#item.id}
    end;
get_id(Tab, Type, Field, undefined) when is_atom(Field) ->
    Id = "id_"++?H(Field),
    case dets:lookup(Tab, Id) of
        [] ->
            Item = #item{id = Id, name = Field, guid = uuid()},
            case Type of
                component ->
                    ok = dets:insert(Tab, Item#item{type=component}),
                    {Item#item.id, Item#item.guid};
                _ ->
                    ok = dets:insert(Tab, Item),
                    {ok, Item#item.guid}
            end;
        [#item{} = Item] ->
            case Type of
                component -> {Item#item.id, Item#item.guid};
                _ -> {ok, Item#item.guid}
            end
    end;
get_id(Tab, Type, F, Dir)
  when Type =:= component;
       Type =:= file;
       Type =:= dir ->
    Id = "id_"++?H({Type, filename:join([Dir, F])}),
    {ok, FI} = file:read_file_info(filename:join([Dir, F])),
    case dets:lookup(Tab, Id) of
        [] ->
            Item = #item{id = Id
                         , type = Type
                         , guid = if Type =:= component -> uuid();
                                     true -> undefined end
                         , name = F
                         , path = Dir
                         , file_info = FI
                        },
            ok = dets:insert(Tab, Item),
            case Type of
                component -> {Item#item.id, Item#item.guid};
                file -> {ok, Item#item.id};
                dir -> {ok, Item#item.id}
            end;
        [#item{name = F, path = Dir, file_info = FI} = Item] ->
            case Type of
                component -> {Item#item.id, Item#item.guid};
                file -> {ok, Item#item.id};
                dir -> {ok, Item#item.id}
            end;
        [#item{name = F, path = Dir} = I] ->
            Item = I#item{guid = if Type =:= component -> uuid();
                                    true -> undefined end
                          , file_info = FI},
            ok = dets:insert(Tab, Item),
            case Type of
                component -> {Item#item.id, Item#item.guid};
                file -> {ok, Item#item.id};
                dir -> {ok, Item#item.id}
            end
    end.
