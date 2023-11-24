suffix=$1
search_dir=$2
for e in $(find $search_dir -type f -name "*.$suffix")
do
    #get realpath
    e=$(realpath $e)
    
    #get the last dir of realpaht
    
    last_dir_name=$(echo $e|awk -F"/" '{print $(NF-1)}')
    
    #get the original filename of realpath
    o_name=$(echo $e|awk -F"/" '{print $(NF)}')
    
    #replace the original name with last_dir
    n_name="${e/$o_name/$last_dir_name}.$suffix"
    
    #rename
    mv $e $n_name
done
