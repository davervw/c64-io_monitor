acme -f cbm -l build/labels -o build/io_monitor.prg code/io_monitor.asm
[ $? -eq 0 ] || exit 1
[ $? -eq 0 ] && c1541 << EOF
attach build/io_monitor.d64
delete "io_monitor"
write build/io_monitor.prg "io_monitor"
EOF
[ $? -eq 0 ] && x64sc -moncommands build/labels build/io_monitor.d64