stack build --profile 
mv pcap-exe.hp pcap-exe_last.hp 
stack exec -- pcap-exe "/Users/HereWegoR/Documents/haskCode/pcap/test/test.pcap" +RTS -h -i0.01 
hp2ps -e8in -c pcap-exe.hp 
open -a Preview pcap-exe.ps 

