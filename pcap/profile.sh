stack build --profile 
mv pcap-exe.prof pcap-exe_last.prof 
stack exec -- pcap-exe "/Users/HereWegoR/Documents/haskCode/pcap/test/test.pcap" +RTS -p 
vim -O -R pcap-exe.prof pcap-exe_last.prof 

