`ifndef TCNT_REPORT_SERVER_BASE__SV
`define TCNT_REPORT_SERVER_BASE__SV

`ifdef TCNT_USE_UVM12
`define UVM_REPORT_SERVER uvm_default_report_server
`else
`define UVM_REPORT_SERVER uvm_report_server
`endif

class tcnt_report_server_base extends `UVM_REPORT_SERVER;

    function new();
        set_name("tcnt_report_server_base");
        $timeformat(-9,3,"ns",10);
    endfunction:new

    `ifndef TCNT_USE_UVM12
    virtual function string compose_message(
        uvm_severity severity,
        string name,
        string id,
        string message,
        string filename,
        int    line
        );
        uvm_severity_type sv;
        string time_str;
        string line_str;
        
        sv = uvm_severity_type'(severity);
        $swrite(time_str, "%0t", $realtime);
 
        case(1)
            (name == "" && filename == ""):
                return {"@", time_str, " ", sv.name(), " [", id, "] ", message};
            (name != "" && filename == ""):
                return {"@", time_str, " ", sv.name(), ": ", name, " [", id, "] ", message};
            (name == "" && filename != ""):
                begin
                    $swrite(line_str, "%0d", line);
                    return {"@", time_str, " ", sv.name(), " ",filename, "(", line_str, ")", " [", id, "] ", message};
                end
            (name != "" && filename != ""):
                begin
                    $swrite(line_str, "%0d", line);
                    return {"@", time_str, " ", sv.name(), " ", filename, "(", line_str, ")", ": ", name, " [", id, "] ", message};
                end
        endcase
    endfunction 
    `endif

    `ifdef TCNT_USE_UVM12
    virtual function string compose_report_message(uvm_report_message report_message,
                                                   string report_object_name = "");
        string sev_string;
        uvm_severity l_severity;
        uvm_verbosity l_verbosity;
        string filename_line_string;
        string time_str;
        string line_str;
        string context_str;
        string verbosity_str;
        string terminator_str;
        string msg_body_str;
        uvm_report_message_element_container el_container;
        string prefix;
        uvm_report_handler l_report_handler;

        l_severity = report_message.get_severity();
        sev_string = l_severity.name();

        if (report_message.get_filename() != "") begin
            line_str.itoa(report_message.get_line());
            filename_line_string = {report_message.get_filename(), "(", line_str, ") "};
        end

        // Make definable in terms of units.
        $swrite(time_str, "%0t", $realtime);
 
        if (report_message.get_context() != "")
            context_str = {"@@", report_message.get_context()};

        if (show_verbosity) begin
            if ($cast(l_verbosity, report_message.get_verbosity()))
                verbosity_str = l_verbosity.name();
            else
                verbosity_str.itoa(report_message.get_verbosity());
            verbosity_str = {"(", verbosity_str, ")"};
        end

        if (show_terminator)
            terminator_str = {" -",sev_string};

        el_container = report_message.get_element_container();
        if (el_container.size() == 0)
            msg_body_str = report_message.get_message();
        else begin
            prefix = uvm_default_printer.knobs.prefix;
            uvm_default_printer.knobs.prefix = " +";
            msg_body_str = {report_message.get_message(), "\n", el_container.sprint()};
            uvm_default_printer.knobs.prefix = prefix;
        end

        if (report_object_name == "") begin
            l_report_handler = report_message.get_report_handler();
            report_object_name = l_report_handler.get_full_name();
        end

        compose_report_message = {sev_string, verbosity_str, " ", "@ ", time_str, " ",  filename_line_string, ": ", report_object_name, context_str, " [", report_message.get_id(), "] ", msg_body_str, terminator_str};
    endfunction 
    `endif
endclass:tcnt_report_server_base

`endif

