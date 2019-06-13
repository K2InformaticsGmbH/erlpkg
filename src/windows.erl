-module(windows).
-include("erlpkg.hrl").

-export([build/1]).

build(#{} = C0) ->
    C1 = C0#{candle => os:find_executable("candle.exe"),
             light => os:find_executable("light.exe")},
    case C1 of
        #{candle := C, light := L} when C == false; L == false ->
            rebar_api:abort(
              "{~p,~p} candle.exe/light.exe not found "
              "make sure http://wixtoolset.org/ is installed and in path",
              [?MODULE, ?LINE]);
        _ -> ok
    end,
    C2 = C1#{tab => list_to_atom(maps:get(rel, C1))},
    ets:new(maps:get(tab, C2), [public, named_table, {keypos, 2}]),
    C3 = case C2 of
             #{msi := MsiConfPath, configDir := ConfDir}
               when is_list(MsiConfPath) ->
                 [MSI] = conf_file:parse_config(?FNJ(ConfDir, MsiConfPath)),
                 C2#{msi => MSI};
             _ -> C2
         end,
    C4 = create_wxs(C3),
    candle_light(C4).
    %C4.

copy_assets(#{pkgDir := PkgDir} = C) ->
    maps:fold(
     fun(K, V, M) when K == icon; K == banner; K == dialog; K == license ->
             Dst = ?FNJ(PkgDir, filename:basename(V)),
             case catch file:copy(V, Dst) of
                 {ok, Bytes} ->
                     ?D("copied ~p bytes from ~s to ~s", [Bytes, V, Dst]),
                     M#{K => filename:basename(V)};
                 Error ->
                     ?D("~s copy to ~s failed : ~p", [V, PkgDir, Error]),
                     ?ABORT("Failed to copy ~s to ~s", [V, PkgDir])
             end;
        (xdlgs, Vs, M) ->
             M#{xdlgs => 
             lists:filtermap(
              fun(IV) ->
                      IDst = ?FNJ(PkgDir, filename:basename(IV)),
                      case catch file:copy(IV, IDst) of
                          {ok, Bytes} ->
                              ?D("copied ~p bytes from ~s to ~s",
                                 [Bytes, IV, IDst]),
                              {true, filename:rootname(filename:basename(IV))};
                          Error ->
                              ?E("~s copy to ~s failed : ~p",
                                 [IV, PkgDir, Error]),
                              false
                      end
              end, Vs)};
        (K, V, M) -> M#{K => V}
     end, #{}, C).

create_wxs(#{rel := Rel, version := Version, pkgDir := PkgDir,
             company := Company, upgradecode := UpgradeCode,
             desc := Comment} = C0) ->
    C1 = start_time(C0, create_wxs),
    ensure_path(PkgDir),
    C2 = copy_assets(C1),
    WxsFile = ?FNJ([PkgDir, lists:flatten([Rel,"-",Version,".wxs"])]),
    ?I("Create ~s", [WxsFile]),
    ?I("---------------------------------------------------------------------"),
    {ok, FileH} = file:open(WxsFile, [write]),
    ?D("FileH ~p", [FileH]),
    C3 = C2#{wxsFileH => FileH},

    {ok, PRODUCT_GUID} = get_id(C3, undefined, 'PRODUCT_GUID', undefined),
    {ok, UPGRADE_GUID} =
    case UpgradeCode of
        '$no_upgrade_code_defined' ->
            get_id(C2, undefined, 'UPGRADE_GUID', undefined);
        UpgradeCode -> {ok, UpgradeCode}
    end,
    {ok, ID} = get_id(C2, undefined, Company, undefined),
    ok = file:write(FileH,
        "<?xml version='1.0' encoding='windows-1252'?>\n"
        "<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'\n"
        "     xmlns:util='http://schemas.microsoft.com/wix/UtilExtension'>\n\n"

        "<Product Name='"++Rel++"'\n"
        "         Id='"++PRODUCT_GUID++"'\n"
        "         UpgradeCode='"++UPGRADE_GUID++"'\n"
        "         Language='1033' Codepage='1252' Version='"++Version++"'\n"
        "         Manufacturer='"++Company++"'>\n\n"

        "   <Package Id='*'\n"
        "            Keywords='Installer'\n"
        "            Description=\""++Company++"\"\n"
        "            Comments='"++Comment++"'\n"
        "            Manufacturer='"++Company++"'\n"
        "            InstallerVersion='200' Languages='1033'\n"
        "            Compressed='yes'\n"
        "            InstallScope='perMachine'\n"
        "            InstallPrivileges='elevated'\n"
        "            SummaryCodepage='1252' />\n\n"

        "   <MajorUpgrade"
                " DowngradeErrorMessage='A later version of [ProductName]"
                    " is already installed. Setup will now exit.' />\n\n"

        "   <Media Id='1' Cabinet='"++Rel++".cab' EmbedCab='yes'\n"
        "          DiskPrompt='CD-ROM #1'/>\n"
        "   <Property Id='DiskPrompt'\n"
        "             Value=\""++Company++" "++Rel
                                             ++" Installation [1]\"/>\n\n"),

    ?I("wxs header sections created"),

    ok = file:write(FileH,
        "   <Directory Id='TARGETDIR' Name='SourceDir'>\n"),

    % AppData PATH
    {CoDatId, CoDatGuId} = get_id(C3, component, 'COMPANYDAT_GUID', undefined),
    {AppDatId, AppDatGuId} = get_id(C3, component, 'PRODUCTDAT_GUID', undefined),
    ok = file:write(FileH,
        "     <Directory Id='CommonAppDataFolder' Name='CommonAppData'>\n"
        "       <Directory Id='COMPANYDAT' Name='"++Company++"'>\n"
        "         <Component Id='"++CoDatId++"' Guid='"++CoDatGuId++"'>\n"
        "           <CreateFolder Directory='COMPANYDAT'>\n"
        "             <Permission User='Everyone' GenericAll='yes' />\n"
        "           </CreateFolder>\n"
        "         </Component>\n"
        "         <Directory Id='PRODUCTDAT' Name='"++Rel++"'>\n"
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
        "     <Directory Id='ProgramFiles64Folder' Name='PFiles'>\n"
        "       <Directory Id='"++ID++"' Name='"++Company++"'>\n"
        "         <Directory Id='INSTALLDIR' Name='"++Rel++"'>\n"),

    walk_release(C3),
    ?I("finished walking OTP release"),

    ok = file:write(FileH,
        "         </Directory> <!-- PRODUCT -->\n"
        "       </Directory> <!-- COMPANY -->\n"
        "     </Directory> <!-- ProgramFiles64Folder -->\n"),

    ?I("finished ProgramFiles64Folder section"),

    % Property references
    [BootDir] = select(C3, [{#item{type=dir, name=Version, _='_'}, [], ['$_']}]),
    [EscriptExe] = select(C3, [{#item{type=component, name="escript.exe",_='_'},
                            [], ['$_']}]),
    [EditConfEs] = select(C3, [{#item{type=component, name="editconfs.escript",
                                  _='_'}, [], ['$_']}]),
    [SrvcCtrlEs] = select(C3, [{#item{type=component, name=Rel++".cmd",
                                  _='_'}, [], ['$_']}]),
    [SrvcCtrlEsFile] = select(C3, [{#item{type=file, name=Rel++".cmd",
                                      guid=undefined, _='_'}, [], ['$_']}]),

    {ProgFolderId, ProgFolderGuId} =
    get_id(C3, component, 'PROGSMENUFOLDER_GUID', undefined),
    {DsktpShortId, DsktpShortGuId} =
    get_id(C3, component, 'DESKTOPSHORTCUT_GUID', undefined),

    ok = file:write(FileH,
        "     <Directory Id='ProgramMenuFolder' Name='Programs'>\n"
        "        <Directory Id='ApplicationProgramMenuFolder'\n"
        "                   Name='"++Rel++"' />\n"
        "     </Directory> <!-- ProgramMenuFolder -->\n\n"),

    ?I("finished ProgramMenuFolder section"),

    ok = file:write(FileH,
        "     <Directory Id='DesktopFolder' Name='Desktop'>\n"
        "       <Directory Id='ApplicationDesktopFolder' Name='"++Rel++"'/>\n"
        "     </Directory> <!-- DesktopFolder -->\n\n"),

    ?I("finished DesktopFolder section"),

    ok = file:write(FileH,
        "   </Directory> <!-- TARGETDIR -->\n\n"),

    ?I("finished TARGETDIR section"),

    build_features(C3),
    ?I("feature sections created"),

    #{license := License, banner := Banner, dialog := Dialog} = C3,
    ok = file:write(FileH,
        "   <WixVariable Id='WixUILicenseRtf' Value='"++License++"' />\n"
        "   <WixVariable Id='WixUIBannerBmp' Value='"++Banner++"' />\n"
        "   <WixVariable Id='WixUIDialogBmp' Value='"++Dialog++"' />\n\n"),

    ?I("added banner and dialog images and license"),

    case C3 of
        #{xdlgs := ExtraDlgs} when length(ExtraDlgs) > 0 ->
            ok = file:write(FileH,
               "   <UIRef Id='WixUI_Mondo' />\n"
               "   <UIRef Id='WixUI_ErrorProgressText' />\n\n"),
            lists:foreach(
              fun(ExtraDlg) ->
                % External Dialog Chaining
                ok = file:write(FileH,
                   "   <UI Id='CustWixUI_Mondo'>\n"
                   "       <UIRef Id='WixUI_Mondo' />\n"
                   "       <UIRef Id='WixUI_ErrorProgressText' />\n\n"

                   "       <DialogRef Id='"++ExtraDlg++"' />\n"

                   "       <Publish Dialog='CustomizeDlg' Control='Next'\n"
                   "                Event='NewDialog' Value='"++ExtraDlg++"'\n"
                   "                Order='3'>LicenseAccepted = 1</Publish>\n"
                   "       <Publish Dialog='VerifyReadyDlg' Control='Back'\n"
                   "                Event='NewDialog' Value='"++ExtraDlg++"'>\n"
                   "           1</Publish>\n"
                   "   </UI>\n\n")
              end, ExtraDlgs);
        _ -> ?I("No custom dlialog chained")
    end,

    ?I("added custom setup dialog"),

    C4 = C3#{msi => conf_file:map(maps:get(msi, C3, #{}), C3)},
    _ = maps:map(
          fun(K, V) ->
            ok = file:write(FileH,
                "   <Property Id='"++string:to_upper(K)++"'>\n"
                "       <RegistrySearch Id='Locate_"++K++"' Root='HKLM'\n"
                "                       Key='Software\\[Manufacturer]\\[ProductName]'\n"
                "                       Name='"++K++"' Type='raw' />\n"
                "       <![CDATA["++V++"]]></Property>\n")
          end, maps:get(msi, C4)),
 
    % Read real installation folder from registry if exists
    ok = file:write(FileH,
        "   <Property Id='INSTALLDIR' Secure='yes'>\n"
        "       <RegistrySearch Id='Locate_INSTALLDIR' Root='HKLM'\n"
        "                       Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                       Name='InstallPath' Type='directory' />\n"
        "   </Property>\n\n"),

    ?I("added properties"),

    %% Service customization
    EscriptExePath = filename:split(EscriptExe#item.path),
    EditConfEsPath = filename:split(EditConfEs#item.path),
    SrvcCtrlEsPath = filename:split(SrvcCtrlEs#item.path),
    EditConfExeCmd = "\"[INSTALLDIR]"
                  ++ string:join(
                       lists:sublist(EscriptExePath, length(EscriptExePath)-1, 2)
                       ++ ["escript.exe"]
                       , "\\")
                  ++ "\" \"[INSTALLDIR]"
                  ++ string:join(
                       lists:sublist(EditConfEsPath, length(EditConfEsPath), 1)
                       ++ ["editconfs.escript"]
                       , "\\")
                  ++ "\"",

    SrvcCommand = "\"[INSTALLDIR]"
                  ++ string:join(
                       lists:sublist(SrvcCtrlEsPath, length(SrvcCtrlEsPath), 1)
                       ++ [Rel++".cmd"]
                       , "\\")
                  ++ "\"",

    ?I("service control ~s @ ~s", [SrvcCommand, BootDir#item.path]),

    %% Service Installation

    % Custom actions service configure
    %  must run after InstallFiles step is 'comitted'
    %  and before InstallService action it must not
    %  impersonate to retain file modification priviledges
    ok = file:write(FileH,
        "   <CustomAction Id='ConfigService' Directory='"++BootDir#item.id++"'\n"
        "                 ExeCommand='"++EditConfExeCmd++" "
                                      "\"\\HKLM\\Software\\[Manufacturer]\\[ProductName]\" "
                                      ++Version++"'\n"
        "                 Execute='commit' Impersonate='no' />\n\n"),

    ?I("added service configuration custom action"),

    % Custom actions service install and start
    %  must run after InstallFiles step is 'comitted'
    ok = file:write(FileH,
        "   <CustomAction Id='InstallService' Directory='"++BootDir#item.id++"'\n"
        "                 ExeCommand='"++SrvcCommand++" install'\n"
        "                 Execute='commit' Impersonate='no' />\n"
        "   <CustomAction Id='StartService' Directory='"++BootDir#item.id++"'\n"
        "                 ExeCommand='"++SrvcCommand++" start'\n"
        "                 Execute='commit' Impersonate='no' />\n"
    % Custom actions service stop and uninstall
    %  must run immediately and before InstallValidate step to ensure that
    %  installed files are not removed and service is stopped before
    %  uninstalling process detecets and warns
    %  Execute='deferred' is MUST to enforce immediate elivated execution
        "   <CustomAction Id='UnInstallService' Directory='"++BootDir#item.id++"'\n"
        "                 ExeCommand='"++SrvcCommand++" uninstall'\n"
        "                 Execute='deferred' Impersonate='no' />\n"
        "   <CustomAction Id='StopService' Directory='"++BootDir#item.id++"'\n"
        "                 ExeCommand='"++SrvcCommand++" stop'\n"
        "                 Execute='deferred' Impersonate='no' />\n\n"),

    ?I("added service control custom actions"),

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

    ?I("added service start/stop sequence for install/uninstall"),

    ok = file:write(FileH,
        "   <DirectoryRef Id='ApplicationProgramMenuFolder'>\n"
        "       <Component Id='"++ProgFolderId++"' Guid='"++ProgFolderGuId++"'>\n"
        "           <Shortcut Id='programattach'\n"
        "                     Name='"++Rel++" Attach'\n"
        "                     Target='[#"++SrvcCtrlEsFile#item.id++"]'\n"
        "                     Arguments='attach'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='APPICON' IconIndex='0' />\n"
        "           <Shortcut Id='programgui'\n"
        "                     Name='"++Rel++" GUI'\n"
        "                     Target='[#"++SrvcCtrlEsFile#item.id++"]'\n"
        "                     Arguments='console'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='APPICON' IconIndex='0' />\n"
        "           <RemoveFolder Id='ApplicationProgramMenuFolder' On='uninstall'/>\n"
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='programmenu' Type='string'\n"
        "                          Value='"++PRODUCT_GUID++"' KeyPath='yes'/>\n"
        % Remember real installation path in registry
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='InstallPath' Type='string'\n"
        "                          Value='[INSTALLDIR]' KeyPath='no'/>\n"),

    _ = maps:map(
          fun(K, _V) ->
            ok = file:write(FileH,
        % Remember all configurable parameters in registry
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'"
                                                                             "\n"
        "                          Name='"++K++"' Type='string'\n"
        "                          Value='["++string:to_upper(K)++"]' KeyPath='no'/>\n")
          end, maps:get(msi, C4)),

    ok = file:write(FileH,
        % Recursively remove application from path
        "           <util:RemoveFolderEx On='uninstall' Property='INSTALLDIR' />\n"
        "       </Component>\n"
        "   </DirectoryRef>\n\n"),

    ?I("added short cuts to ApplicationProgramMenuFolder"),

    ok = file:write(FileH,
        "   <DirectoryRef Id='ApplicationDesktopFolder'>\n"
        "       <Component Id='"++DsktpShortId++"' Guid='"++DsktpShortGuId++"'>\n"
        "           <Shortcut Id='desktopattach'\n"
        "                     Name='"++Rel++" Attach'\n"
        "                     Target='[#"++SrvcCtrlEsFile#item.id++"]'\n"
        "                     Arguments='attach'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='APPICON' IconIndex='0' />\n"
        "           <Shortcut Id='desktopgui'\n"
        "                     Name='"++Rel++" GUI'\n"
        "                     Target='[#"++SrvcCtrlEsFile#item.id++"]'\n"
        "                     Arguments='console'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='APPICON' IconIndex='0' />\n"
        "           <RemoveFolder Id='ApplicationDesktopFolder' On='uninstall'/>\n"
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='desktop' Type='integer'\n"
        "                          Value='1' KeyPath='yes'/>\n"
        "       </Component>\n"
        "   </DirectoryRef>\n"),

    ?I("added short cuts to ApplicationDesktopFolder"),

    #{icon := AppIcon} = C3,
    ok = file:write(FileH,
        "   <Icon Id='APPICON' SourceFile='"++AppIcon++"' />\n"),

    ok = file:write(FileH,
        "   <Property Id='ARPPRODUCTICON' Value='APPICON' />"),

    ok = file:write(FileH,
        "</Product>\n"
        "</Wix>"),

    ?I("finised building wxs"),

    ok = file:close(FileH),
    ?I("---------------------------------------------------------------------"),
    end_time(C3, create_wxs).

ensure_path(Path) ->
    case filelib:is_dir(Path) of
        false ->
            case file:make_dir(Path) of
                {error, enoent} ->
                    [_|Rest] = lists:reverse(filename:split(Path)),
                    ensure_path(filename:join(lists:reverse(Rest))),
                    ensure_path(Path);
                {error, Error} -> ?ABORT("failed to create ~p : ~p",
                                         [Path, Error]);
                ok -> ok
            end;
        true -> ok
    end.

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

get_id(C, undefined, Field, undefined) when is_list(Field) ->
    Id = "id_"++?H(Field),
    case lookup(C, Id) of
        [] ->
            Item = #item{id = Id, name = Field},
            insert(C, Item),
            {ok, Item#item.id};
        [#item{} = Item] ->
            {ok, Item#item.id}
    end;
get_id(C, Type, Field, undefined) when is_atom(Field) ->
    Id = "id_"++?H(Field),
    case lookup(C, Id) of
        [] ->
            Item = #item{id = Id, name = Field, guid = uuid()},
            case Type of
                component ->
                    insert(C, Item#item{type=component}),
                    {Item#item.id, Item#item.guid};
                _ ->
                    insert(C, Item),
                    {ok, Item#item.guid}
            end;
        [#item{} = Item] ->
            case Type of
                component -> {Item#item.id, Item#item.guid};
                _ -> {ok, Item#item.guid}
            end
    end;
get_id(C, Type, F, Dir)
  when Type =:= component;
       Type =:= file;
       Type =:= dir ->
    Id = "id_"++?H({Type, ?FNJ([Dir, F])}),
    {ok, FI} = file:read_file_info(?FNJ([Dir, F])),
    case lookup(C, Id) of
        [] ->
            Item = #item{id = Id
                         , type = Type
                         , guid = if Type =:= component -> uuid();
                                     true -> undefined end
                         , name = F
                         , path = Dir
                         , file_info = FI
                        },
            insert(C, Item),
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
            insert(C, Item),
            case Type of
                component -> {Item#item.id, Item#item.guid};
                file -> {ok, Item#item.id};
                dir -> {ok, Item#item.id}
            end;
        Items ->
            ?ABORT("CLASH ~p with ~p", [{Type, F, Dir, Id}, Items])
    end.

select(#{tab := Tab}, MatchSpec) -> ets:select(Tab, MatchSpec).
insert(#{tab := Tab}, Item) -> true = ets:insert(Tab, Item).
lookup(#{tab := Tab}, Id) -> ets:lookup(Tab, Id).
foreach(#{tab := Tab}, Fun) when is_function(Fun, 1) ->
    ets:foldl(fun(Row, '$unused') ->
                      Fun(Row),
                      '$unused'
              end, '$unused', Tab).

uuid() ->
    <<U0:32, U1:16, _:4, U2:12, _:2, U3:30, U4:32>> =
    crypto:strong_rand_bytes(16),
    <<X0:32, X1:16, X2:16, X3:16, X4:48>> =
    <<U0:32, U1:16, 4:4, U2:12, 2#10:2, U3:30, U4:32>>,
    string:to_upper(
      lists:flatten(
        io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                      [X0, X1, X2, X3, X4]))).

walk_release(#{relAppDir := ReleaseRoot} = C) ->
    case filelib:is_dir(ReleaseRoot) of
        true ->
            walk_release(
              C, length(ReleaseRoot)+2,
              filelib:wildcard("*", ReleaseRoot), ReleaseRoot, 12);
        false -> ?ABORT("~p is not a directory", [ReleaseRoot])
    end.

walk_release(#{wxsFileH := FileH} = C, PathPrefixLen, Files, Dir, N) ->
    lists:map(
      fun(F) ->
        ProcessPath = ?FNJ([Dir,F]),
        case filelib:is_dir(ProcessPath) of
            true ->
                FilesAtThisLevel = filelib:wildcard("*", ProcessPath),
                {ok, DirId} = get_id(C, dir, F, Dir),
                ?D("~s/", [string:substr(ProcessPath, PathPrefixLen)]),
                Indent = lists:duplicate(N,32),
                ok = file:write(
                       FileH,
                       [Indent, "<Directory Id='", DirId, "' Name='", F, "'>\n"]),

                walk_release(C, PathPrefixLen, FilesAtThisLevel, ProcessPath, N+3),
                ok = file:write(FileH, [Indent, "</Directory>\n"]);
            false ->
                FilePath = get_filepath(Dir, F),
                {Id, GuID} = get_id(C, component, F, Dir),
                {ok, FileId} = get_id(C, file, F, Dir),
                Content = [lists:duplicate(N+3,32),
                         "<Component Id='",Id,"' Guid='",GuID,"'>\n",lists:duplicate(N+3,32),
                         "   <File Id='",FileId,"' Name='",F,
                         "' DiskId='1' Source='",FilePath,"'"
                         " KeyPath='yes' />\n",lists:duplicate(N+3,32),
                         "</Component>\n"],
                ok = file:write(FileH, Content)
        end
      end, Files).

get_filepath(Dir, F) ->
    FilePathNoRel =
        lists:foldl(
          fun
              ("..", Acc) -> Acc;
              (P,Acc) -> Acc ++ [P]
          end,
          [], filename:split(Dir)),
    ?FNJ([".." | FilePathNoRel]++[F]).

build_features(#{wxsFileH := FileH, rel := Rel, version := Version} = C) ->
    ok = file:write(FileH,
        "   <Feature Id='Complete' Title='"++Rel++"-"++Version++"'"
                    " Description='The complete package.'"
                    " Level='1' ConfigurableDirectory='INSTALLDIR'>\n"),
    ok = file:write(FileH,
        "      <Feature Id='MainProgram' Title='"++Rel++"-"++Version
                                                        ++" service'"
                    " Description='The service.' Level='1'>\n"),
    foreach(
      C,
      fun(#item{type=component, id = Id}) ->
              ok = file:write(FileH, "         <ComponentRef Id='"++Id++"' />\n");
         (_) -> ok
      end),
    ok = file:write(FileH, "      </Feature>\n\n"),
    ok = file:write(FileH, "   </Feature>\n\n").

candle_light(#{candle := Candle, light := Light, pkgDir := PkgDir} = C) ->
    C1 = start_time(C, candle_light),
    {ok, CurDir} = file:get_cwd(),
    ok = file:set_cwd(PkgDir),
    Wxses = filelib:wildcard("*.wxs"),
    ?I("candle ~p", [Wxses]),
    erlpkg_utils:run_port(Candle, ["-v", "-arch", "x64", "-ext", "WixUtilExtension" | Wxses]),
    WixObjs = filelib:wildcard("*.wixobj"),
    MsiFile = generate_msi_name(C1),
    ?I("light ~p -> ~s", [WixObjs, MsiFile]),
    erlpkg_utils:run_port(
      Light, ["-v", "-ext", "WixUtilExtension", "-ext", "WixUIExtension",
              "-out", MsiFile | WixObjs]),
    ok = file:set_cwd(CurDir),
    end_time(C1, candle_light).

generate_msi_name(#{rel := Rel, version := Version,
                    patchCode := PatchCode}) ->
    {{Y,M,D},{H,Mn,S}} = calendar:local_time(),
    MsiDate = io_lib:format("~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B",
                            [Y,M,D,H,Mn,S]),
    lists:flatten([Rel,"-", Version,".", PatchCode,"_", MsiDate,".msi"]).

-ifdef(FINISHED).

gen_patch_ts() ->
    C = get(config),
    BeamPath = filename:join(C#config.appRelDir, "ebin"),
    [{{{_,Month,Day},{Hour,_,_}},_}|_]
    = lists:reverse(
        lists:usort(
          [{filelib:last_modified(filename:join(BeamPath, B)),
            B} || B <- filelib:wildcard("*.beam", BeamPath)]
         )),
    put(config,
        C#config{
          patchCode = lists:flatten(
                        io_lib:format(
                          "~2..0B~2..0B~2..0B",
                          [Month,Day,Hour])
                       )}).

print_stats() ->
    C = get(config),
    ?L("---------------------------------------------------------------------"),
    ?L("total build time ~s", [ft(maps:get(total, C#config.stats))]),
    ?L("---------------------------------------------------------------------"),
    maps:fold(fun(K, V, _) when K /= total -> ?L("~p time ~s", [K, ft(V)]);
                 (_, _, _) -> undefined
              end, undefined, C#config.stats),
    ?L("---------------------------------------------------------------------").

ft(T) when T < 1000 -> integer_to_list(T)++"us";
ft(T) when T >= 1000, T < 1000000 -> integer_to_list(T div 1000)++"ms";
ft(T) when T >= 1000000 -> integer_to_list(T div 1000000)++"s".

timestamp() ->
    TS = {_,_,Micro} = os:timestamp(),
    {{Year,Month,Day},{Hour,Minute,Second}}
    = calendar:now_to_universal_time(TS),
    io_lib:format("~2..0w~2..0w~4..0w-~2..0w~2..0w~2..0w.~s",
                  [Day,Month,Year,Hour,Minute,Second,
                   string:left(integer_to_list(Micro), 3, $0)]).
-endif. % FINISHED
