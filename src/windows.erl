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
    %case file:copy(?FNJ([C2#config.scriptDir,"service.escript"]),
    %               ?FNJ([C2#config.relDir,"bin",C#config.app++".escript"])) of
    %    {error, Error} ->
    %        exit({"failed to copy service.escript", Error});
    %    _ -> ok
    %end,
    C2 = C1#{tab => list_to_atom(maps:get(app, C1))},
    ets:new(maps:get(tab, C2), [public, named_table, {keypos, 2}]),
    create_wxs(C2).

create_wxs(#{app := Proj, version := Version, pkgDir := PkgDir,
             company := Company, upgradecode := UpgradeCode, desc := Comment} = C0) ->
    C1 = start_time(C0, create_wxs),
    ensure_path(PkgDir),
    WxsFile = filename:join([PkgDir, lists:flatten([Proj,"-",Version,".wxs"])]),
    ?I("Create ~s", [WxsFile]),
    ?I("--------------------------------------------------------------------------------"),
    {ok, FileH} = file:open(WxsFile, [write]),
    ?D("FileH ~p", [FileH]),
    C = C1#{wxsFileH => FileH},

    {ok, PRODUCT_GUID} = get_id(C, undefined, 'PRODUCT_GUID', undefined),
    {ok, UPGRADE_GUID} = case UpgradeCode of
                             '$no_upgrade_code_defined' ->
                                 get_id(C, undefined, 'UPGRADE_GUID', undefined);
                             UpgradeCode -> {ok, UpgradeCode}
                         end,
    {ok, ID} = get_id(C, undefined, Company, undefined),
    ok = file:write(FileH,
        "<?xml version='1.0' encoding='windows-1252'?>\n"
        "<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'\n"
        "     xmlns:util='http://schemas.microsoft.com/wix/UtilExtension'>\n\n"

        "<Product Name='"++Proj++"'\n"
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

        "   <MajorUpgrade DowngradeErrorMessage='A later version of [ProductName]"
                             " is already installed. Setup will now exit.' />\n\n"

        "   <Media Id='1' Cabinet='"++Proj++".cab' EmbedCab='yes'\n"
        "          DiskPrompt='CD-ROM #1'/>\n"
        "   <Property Id='DiskPrompt'\n"
        "             Value=\""++Company++" "++Proj++" Installation [1]\"/>\n\n"),

    ?I("wxs header sections created"),

    ok = file:write(FileH,
        "   <Directory Id='TARGETDIR' Name='SourceDir'>\n"),

    % AppData PATH
    {CoDatId, CoDatGuId} = get_id(C, component, 'COMPANYDAT_GUID', undefined),
    {AppDatId, AppDatGuId} = get_id(C, component, 'PRODUCTDAT_GUID', undefined),
    ok = file:write(FileH,
        "     <Directory Id='CommonAppDataFolder' Name='CommonAppData'>\n"
        "       <Directory Id='COMPANYDAT' Name='"++Company++"'>\n"
        "         <Component Id='"++CoDatId++"' Guid='"++CoDatGuId++"'>\n"
        "           <CreateFolder Directory='COMPANYDAT'>\n"
        "             <Permission User='Everyone' GenericAll='yes' />\n"
        "           </CreateFolder>\n"
        "         </Component>\n"
        "         <Directory Id='PRODUCTDAT' Name='"++Proj++"'>\n"
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
        "         <Directory Id='INSTALLDIR' Name='"++Proj++"'>\n"),

    walk_release(C),
    ?I("finished walking OTP release"),

    ok = file:write(FileH,
        "         </Directory> <!-- PRODUCT -->\n"
        "       </Directory> <!-- COMPANY -->\n"
        "     </Directory> <!-- ProgramFiles64Folder -->\n"),

    ?I("finished ProgramFiles64Folder section"),

    % Property references
    [BootDir] = select(C, [{#item{type=dir, name=Version, _='_'}, [], ['$_']}]),
    [EscriptExe] = select(C, [{#item{type=component, name="escript.exe",_='_'},
                            [], ['$_']}]),
    [EscriptExeFile] = select(C, [{#item{type=file, name="escript.exe",
                                      guid=undefined, _='_'}, [], ['$_']}]),
    [EditConfEs] = select(C, [{#item{type=component, name="editconfs.escript",
                                  _='_'}, [], ['$_']}]),
    [SrvcCtrlEs] = select(C, [{#item{type=component, name=Proj++".cmd",
                                  _='_'}, [], ['$_']}]),
    [SrvcCtrlEsFile] = select(C, [{#item{type=file, name=Proj++".cmd",
                                      guid=undefined, _='_'}, [], ['$_']}]),

    {ProgFolderId, ProgFolderGuId} = get_id(C, component, 'PROGSMENUFOLDER_GUID', undefined),
    {DsktpShortId, DsktpShortGuId} = get_id(C, component, 'DESKTOPSHORTCUT_GUID', undefined),

    ok = file:write(FileH,
        "     <Directory Id='ProgramMenuFolder' Name='Programs'>\n"
        "        <Directory Id='ApplicationProgramMenuFolder'\n"
        "                   Name='"++Proj++"' />\n"
        "     </Directory> <!-- ProgramMenuFolder -->\n\n"),

    ?I("finished ProgramMenuFolder section"),

    ok = file:write(FileH,
        "     <Directory Id='DesktopFolder' Name='Desktop'>\n"
        "       <Directory Id='ApplicationDesktopFolder' Name='"++Proj++"'/>\n"
        "     </Directory> <!-- DesktopFolder -->\n\n"),

    ?I("finished DesktopFolder section"),

    ok = file:write(FileH,
        "   </Directory> <!-- TARGETDIR -->\n\n"),

    ?I("finished TARGETDIR section"),

    build_features(C),
    ?I("feature sections created"),

    ok = file:write(FileH,
        "   <WixVariable Id='WixUILicenseRtf' Value='License.rtf' />\n"
        "   <WixVariable Id='WixUIBannerBmp' Value='banner493x58.jpg' />\n"
        "   <WixVariable Id='WixUIDialogBmp'"
                       " Value='dialog493x312.jpg' />\n\n"),

    ?I("added banner and dialog images and license"),

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

    ?I("added custom setup dialog"),

    [VmArgsFile] = select(C, [{#item{type=file, name="vm.args", _='_'}, [],
                            ['$_']}]),
    [SysConfigFile] = select(C, [{#item{type=file, name="sys.config", _='_'}, [],
                               ['$_']}]),
    {ok, VmArgsBin} = file:read_file(?FNJ(VmArgsFile#item.path, "vm.args")),
    {match, [Node]} = re:run(VmArgsBin
                             , ".*-name (.*)[\r\n]"
                             , [{capture, [1], list}, ungreedy, dotall]),
    {match, [Cookie]} = re:run(VmArgsBin
                               , ".*-setcookie (.*)[\r\n]"
                               , [{capture, [1], list}, ungreedy, dotall]),

    {ok, [SysConfigs]} = file:consult(?FNJ(SysConfigFile#item.path, "sys.config")),

    DDErl = proplists:get_value(dderl, SysConfigs),
    DDErlIntf = proplists:get_value(interface, DDErl),
    DDErlPort = integer_to_list(proplists:get_value(port, DDErl)),

    Imem = proplists:get_value(imem, SysConfigs),
    ImemNodeType = atom_to_list(proplists:get_value(mnesia_node_type, Imem)),
    ImemSchemaName = atom_to_list(proplists:get_value(mnesia_schema_name, Imem)),
    ImemClustMgrs = lists:flatten(io_lib:format("~p", [proplists:get_value(erl_cluster_mgrs, Imem)])),
    ImemIntf = proplists:get_value(tcp_ip, Imem),
    ImemPort = integer_to_list(proplists:get_value(tcp_port, Imem)),
    ImemNodeShardFun0 = lists:flatten(io_lib:format("~p", [proplists:get_value(node_shard_fun, Imem)])),
    [$"|ImemNodeShardFun1] = ImemNodeShardFun0,
    [$"|ImemNodeShardFun2] = lists:reverse(ImemNodeShardFun1),
    ImemNodeShardFun = lists:reverse(ImemNodeShardFun2),

    ok = file:write(FileH,
        "   <Property Id='NODENAME'>\n"
        "       <RegistrySearch Id='Locate_NODENAME' Root='HKLM'\n"
        "                       Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                       Name='NodeName' Type='raw' />\n"++Node++
        "   </Property>\n"
        "   <Property Id='NODECOOKIE'>\n"
        "       <RegistrySearch Id='Locate_NODECOOKIE' Root='HKLM'\n"
        "                       Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                       Name='NodeCookie' Type='raw' />\n"++Cookie++
        "   </Property>\n"
        "   <Property Id='WEBSRVINTF'>\n"
        "       <RegistrySearch Id='Locate_WEBSRVINTF' Root='HKLM'\n"
        "                       Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                       Name='WebSrvIntf' Type='raw' />\n"++DDErlIntf++":"++DDErlPort++
        "   </Property>\n"
        "   <Property Id='DBNODETYPE'>\n"
        "       <RegistrySearch Id='Locate_DBNODETYPE' Root='HKLM'\n"
        "                       Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                       Name='DbNodeType' Type='raw' />\n"++ImemNodeType++
        "   </Property>\n"
        "   <Property Id='DBNODETYPE_DISC'>disc</Property>\n"
        "   <Property Id='DBNODETYPE_RAM'>ram</Property>\n"
        "   <Property Id='DBNODESCHEMANAME'>\n"
        "       <RegistrySearch Id='Locate_DBNODESCHEMANAME' Root='HKLM'\n"
        "                       Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                       Name='DbNodeSchemaName' Type='raw' />\n"++ImemSchemaName++
        "   </Property>\n"
        "   <Property Id='DBCLUSTERMGRS'>\n"
        "       <RegistrySearch Id='Locate_DBCLUSTERMGRS' Root='HKLM'\n"
        "                       Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                       Name='DbClusterManagers' Type='raw' />\n"
        "       <![CDATA["++ImemClustMgrs++"]]></Property>\n"
        "   <Property Id='DBNODESHARDFUN'>\n"
        "       <RegistrySearch Id='Locate_DBNODESHARDFUN' Root='HKLM'\n"
        "                       Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                       Name='DbNodeShardFunction' Type='raw' />\n"
        "       <![CDATA["++ImemNodeShardFun++"]]></Property>\n"
        "   <Property Id='DBINTF'>\n"
        "       <RegistrySearch Id='Locate_DBINTF' Root='HKLM'\n"
        "                       Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                       Name='DbInterface' Type='raw' />\n"++ImemIntf++":"++ImemPort++
        "   </Property>\n\n"),

    % Read real installation folder from registry if exists
    ok = file:write(FileH,
        "   <Property Id='INSTALLDIR' Secure='yes'>\n"
        "       <RegistrySearch Id='Locate_INSTALLDIR' Root='HKLM'\n"
        "                       Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                       Name='InstallPath' Type='directory' />\n"
        "   </Property>\n\n"),

    ?I("added properties connecting to custom setup dialog"),

    %% Service customization
    EscriptExePath = filename:split(EscriptExe#item.path),
    EditConfEsPath = filename:split(EditConfEs#item.path),
    SrvcCtrlEsPath = filename:split(SrvcCtrlEs#item.path),
    ExecCommand = "\"[INSTALLDIR]"
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
                       ++ [Proj++".cmd"]
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
        "                 ExeCommand='"++ExecCommand++" \"[NODENAME]\" "
                                      "\"[NODECOOKIE]\" \"[WEBSRVINTF]\" "
                                      "\"[DBNODETYPE]\" \"[DBNODESCHEMANAME]\" "
                                      "\"[DBCLUSTERMGRS]\" \"[DBINTF]\" "
                                      "\"[DBNODESHARDFUN]\" "
                                      "\"["++BootDir#item.id++"]\\\" "
                                      "\"[PRODUCTDAT]\\\"'\n"
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
        "                     Name='"++Proj++" Attach'\n"
        "                     Target='[#"++SrvcCtrlEsFile#item.id++"]'\n"
        "                     Arguments='attach'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='application.ico' IconIndex='0' />\n"
        "           <Shortcut Id='programgui'\n"
        "                     Name='"++Proj++" GUI'\n"
        "                     Target='[#"++SrvcCtrlEsFile#item.id++"]'\n"
        "                     Arguments='console'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='application.ico' IconIndex='0' />\n"
        "           <RemoveFolder Id='ApplicationProgramMenuFolder' On='uninstall'/>\n"
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='programmenu' Type='string'\n"
        "                          Value='"++PRODUCT_GUID++"' KeyPath='yes'/>\n"
        % Remember real installation path in registry
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='InstallPath' Type='string'\n"
        "                          Value='[INSTALLDIR]' KeyPath='no'/>\n"
        % Remember all configurable parameters in registry
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='NodeName' Type='string'\n"
        "                          Value='[NODENAME]' KeyPath='no'/>\n"
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='NodeCookie' Type='string'\n"
        "                          Value='[NODECOOKIE]' KeyPath='no'/>\n"
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='WebSrvIntf' Type='string'\n"
        "                          Value='[WEBSRVINTF]' KeyPath='no'/>\n"
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='DbNodeType' Type='string'\n"
        "                          Value='[DBNODETYPE]' KeyPath='no'/>\n"
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='DbNodeSchemaName' Type='string'\n"
        "                          Value='[DBNODESCHEMANAME]' KeyPath='no'/>\n"
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='DbClusterManagers' Type='string'\n"
        "                          Value='[DBCLUSTERMGRS]' KeyPath='no'/>\n"
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='DbInterface' Type='string'\n"
        "                          Value='[DBINTF]' KeyPath='no'/>\n"
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='DbNodeShardFunction' Type='string'\n"
        "                          Value='[DBNODESHARDFUN]' KeyPath='no'/>\n"
        % Recursively remove application from path
        "           <util:RemoveFolderEx On='uninstall' Property='INSTALLDIR' />\n"
        "       </Component>\n"
        "   </DirectoryRef>\n\n"),

    ?I("added short cuts to ApplicationProgramMenuFolder"),

    ok = file:write(FileH,
        "   <DirectoryRef Id='ApplicationDesktopFolder'>\n"
        "       <Component Id='"++DsktpShortId++"' Guid='"++DsktpShortGuId++"'>\n"
        "           <Shortcut Id='desktopattach'\n"
        "                     Name='"++Proj++" Attach'\n"
        "                     Target='[#"++SrvcCtrlEsFile#item.id++"]'\n"
        "                     Arguments='attach'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='application.ico' IconIndex='0' />\n"
        "           <Shortcut Id='desktopgui'\n"
        "                     Name='"++Proj++" GUI'\n"
        "                     Target='[#"++SrvcCtrlEsFile#item.id++"]'\n"
        "                     Arguments='console'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='application.ico' IconIndex='0' />\n"
        "           <RemoveFolder Id='ApplicationDesktopFolder' On='uninstall'/>\n"
        "           <RegistryValue Root='HKLM'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='desktop' Type='integer'\n"
        "                          Value='1' KeyPath='yes'/>\n"
        "       </Component>\n"
        "   </DirectoryRef>\n"),

    ?I("added short cuts to ApplicationDesktopFolder"),

    ok = file:write(FileH,
        "   <Icon Id='application.ico' SourceFile='application.ico' />\n"),

    ok = file:write(FileH,
        "   <Property Id='ARPPRODUCTICON' Value='application.ico' />"),

    ok = file:write(FileH,
        "</Product>\n"
        "</Wix>"),

    ?I("finised building wxs"),

    ok = file:close(FileH),
    C2 = end_time(C, create_wxs),
    ?I("--------------------------------------------------------------------------------"),
    C2.

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
            C#{stats => Stats#{stats => timer:now_diff(os:timestamp(), Start)}};
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
                Content = [lists:duplicate(N,32), "<Directory Id='", DirId, "' Name='", F, "'>\n",
                         walk_release(C, PathPrefixLen, FilesAtThisLevel, ProcessPath, N+3),
                         lists:duplicate(N,32), "</Directory>\n"],
                case catch file:write(FileH, Content) of
                    ok -> "";
                    Error -> ?E("~p to write ~p", [Error, Content])
                end;
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
                case catch file:write(FileH, Content) of
                    ok -> "";
                    Error -> ?E("~p to write ~p", [Error, Content])
                end
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

build_features(#{wxsFileH := FileH, app := Proj, version := Version} = C) ->
    ok = file:write(FileH,
        "   <Feature Id='Complete' Title='"++Proj++"-"++Version++"'"
                    " Description='The complete package.'"
                    " Level='1' ConfigurableDirectory='INSTALLDIR'>\n"),
    ok = file:write(FileH,
        "      <Feature Id='MainProgram' Title='"++Proj++"-"++Version
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

-ifdef(FINISHED).

candle_light() ->
    start_time(candle_light),
    Verbose = get(verbose),
    C = get(config),
    {ok, CurDir} = file:get_cwd(),
    ok = file:set_cwd(C#config.topDir),
    Wxses = filelib:wildcard("*.wxs"),
    ?I("candle with ~p", [Wxses]),
    run_port(C#config.candle, if Verbose -> ["-v"]; true -> [] end
             ++ ["-arch", "x64", "-ext", "WixUtilExtension" | Wxses]),
    WixObjs = filelib:wildcard("*.wixobj"),
    MsiFile = generate_msi_name(),
    ?I("light ~s with ~p", [MsiFile, WixObjs]),
    run_port(C#config.light, if Verbose -> ["-v"]; true -> [] end
             ++ ["-ext", "WixUtilExtension",
                 "-ext", "WixUIExtension",
                 "-out", MsiFile | WixObjs]),
    ok = file:set_cwd(CurDir),
    end_time(candle_light).

run_port(Cmd, Args) ->
    log_cmd(Cmd,
            erlang:open_port(
              {spawn_executable,Cmd},
              [{line, 128},{args, Args}, exit_status,
               stderr_to_stdout, {parallelism, true}])).
run_port(Cmd, Args, Cwd) ->
    ?Lg("run_port(~p, ~p, ~p)", [Cmd, Args, Cwd]),
    log_cmd(Cmd,
            erlang:open_port(
              {spawn_executable,Cmd},
              [{cd,Cwd},{line,128},{args,Args},exit_status,stderr_to_stdout,
               {parallelism,true}])).

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
    ?L("--------------------------------------------------------------------------------"),
    ?L("total build time ~s", [ft(maps:get(total, C#config.stats))]),
    ?L("--------------------------------------------------------------------------------"),
    maps:fold(fun(K, V, _) when K /= total -> ?L("~p time ~s", [K, ft(V)]);
                 (_, _, _) -> undefined
              end, undefined, C#config.stats),
    ?L("--------------------------------------------------------------------------------").

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

generate_msi_name() ->
    C = get(config),
    {{Y,M,D},{H,Mn,S}} = calendar:local_time(),
    MsiDate = io_lib:format("~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B",
                            [Y,M,D,H,Mn,S]),
    lists:flatten([C#config.app,"-",
                   C#config.version,".", C#config.patchCode,"_",
                   MsiDate,".msi"]).

-endif. % FINISHED
