#! /usr/bin/csh
#set files = (`ls bus_handler_*.sv`)
#echo $files
#
#foreach f ($files)
#    set fname = "iommu_${f}"
#    echo "${fname}.sv"
#    mv $f "${fname}.sv"
#end
#
#
#set bfiles = (`ls *.sv.sv`)
#
#foreach fn ($bfiles)
#    set ff1 = $fn:r
#    echo $ff1
#    set ff = ${ff1}.sv
#    echo $ff
#    #mv $fn ${ff}
#    mv $fn $ff1
#end


set dofiles = (`ls iommu_*.sv`)

foreach fl ($dofiles)
    sed -i 's/BUS_INFLY/BUS_ONFLY/g' $fl
end
