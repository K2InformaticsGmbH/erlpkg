# erlpkg
Erlang OTP application multi platform deployment packaging scripts

## Example OTP Application structure (with release)
```
|-- deps
|   `-- erlpkg
|       |-- LICENSE
|       |-- README.md
|       |-- common.erl
|       |-- common.hrl
|       |-- ebin
|       |-- linux
|       |   |-- build_rpm.escript
|       |   |-- erlpkg.conf
|       |   `-- remove_deps.escript
|       `-- windows
|           |-- License.rtf
|           |-- ServiceSetupDlg.wxs
|           |-- application.ico
|           |-- banner493x58.jpg
|           |-- build_msi.escript
|           |-- dialog493x312.jpg
|           |-- erlpkg.conf
|           `-- service.escript
|-- ebin
|   |-- demo.app
|   |-- demo_app.beam
|   `-- demo_sup.beam
|-- rebar.config
|-- rel
|   |-- demo
|   |   |-- bin
|   |   |   |-- demo
|   |   |   |-- demo.cmd
|   |   |   |-- start_clean.boot
|   |   |   `-- start_erl.cmd
|   |   |-- erts-6.4
|   |   |   `-- bin
|   |   |        ...      
|   |   |-- lib
|   |   |   |-- asn1-3.0.4
|   |   |   |   |-- ebin
|   |   |   |   |    ...
|   |   |   |   `-- priv
|   |   |   |       `-- lib
|   |   |   |           `-- asn1rt_nif.dll
...
|   |   |   |
|   |   |   `-- wx-1.3.3
|   |   |       |-- ebin
|   |   |       |    ...
|   |   |       |-- include
|   |   |       |   |-- gl.hrl
|   |   |       |   |-- glu.hrl
|   |   |       |   `-- wx.hrl
|   |   |       `-- priv
|   |   |           |-- erl_gl.dll
|   |   |           |-- erlang-logo32.png
|   |   |           |-- erlang-logo64.png
|   |   |           `-- wxe_driver.dll
|   |   |-- log
|   |   |   `-- sasl
|   |   `-- releases
|   |       |-- 1
|   |       |   |-- demo.boot
|   |       |   |-- demo.rel
|   |       |   |-- demo.script
|   |       |   |-- nodetool
|   |       |   |-- start_clean.boot
|   |       |   |-- start_clean.rel
|   |       |   `-- start_clean.script
|   |       |-- RELEASES
|   |       `-- start_erl.data
|   |-- files
|   |   |-- demo
|   |   |-- demo.cmd
|   |   |-- erl
|   |   |-- install_upgrade.escript
|   |   |-- nodetool
|   |   |-- start_erl.cmd
|   |   |-- sys.config
|   |   `-- vm.args
|   `-- reltool.config
`-- src
    |-- demo.app.src
    |-- demo_app.erl
    `-- demo_sup.erl
```
