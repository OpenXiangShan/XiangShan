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


set dofiles = (`ls *.sv`)

foreach fl ($dofiles)
    set newfile = `echo $fl | sed 's/iommu_atd_dtc/iommu_atd_ptc/'`
    cp $fl $newfile
end
