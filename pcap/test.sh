stack build
rm ./raw 

echo "test raw parsing" 
stack exec -- pcap-exe "/Users/HereWegoR/Documents/haskCode/pcap/test/test.pcap" > raw 
diff ./raw regression/raw 

echo "test sorting"
stack exec -- pcap-exe "/Users/HereWegoR/Documents/haskCode/pcap/test/test.pcap" -r > sorted 
diff ./sorted regression/sorted 


