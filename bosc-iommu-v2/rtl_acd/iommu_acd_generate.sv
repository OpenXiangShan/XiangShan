module iommu_acd_std_and(
    input  wire         A1,
    input  wire         A2,
    output wire         Z
);

    assign Z = A1 & A2;

endmodule
