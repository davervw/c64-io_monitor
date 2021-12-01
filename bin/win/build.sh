export ACME=/c/Users/Dave/Downloads/acme0.96.4win/acme/ACME_Lib 
export VICE=/c/Users/Dave/Downloads/GTK3VICE-3.3-win32/GTK3VICE-3.3-win32-r35872
bin/win/acme -f cbm -l build/labels -o build/io_monitor.prg code/io_monitor.asm
[ $? -eq 0 ] || exit 1
[ $? -eq 0 ] && ${VICE}/c1541 << EOF
attach build/io_monitor.d64
delete "io_monitor"
write build/io_monitor.prg "io_monitor"
EOF
[ $? -eq 0 ] && ${VICE}/x64.exe -moncommands build/labels build/io_monitor.d64